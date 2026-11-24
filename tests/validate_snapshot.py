"""Validate original Turkey snapshot contract, with explicit W extension."""
import datetime as dt, json, math, re, sys
from pathlib import Path
p = Path(sys.argv[1])
j=json.loads(p.read_text(encoding='utf-8'))
assert isinstance(j.get('origen'),dict)
required = {'titulo_serie','subtitulo_descripcion','transformacion_y_calculo','fuente','frecuencia_y_cobertura_temporal','tipo_grafico','bloque','área','frecuencia','unidad','observaciones'}
core = {'COMMERCIAL','CONSUMER','GPL','AUTO','MORTGAGE','DEPOSIT','FUNDING','REPO','COMM_SPREAD','CONS_SPREAD','CPI_HEADLINE','CPI_CORE_B','CPI_CORE_C','CPI_GOODS','CPI_SERVICES'} | {x+s for x in ('BOP_CAD','BOP_FIN','BOP_NEO','BOP_RES','TRADE_EXPORT','TRADE_IMPORT','TRADE_BALANCE') for s in ('','_12M')}
core |= {'TREND_'+x for x in ('CPI','CORE_B','CORE_C','GOODS','SERVICES')}
assert {'TR_'+x for x in core} <= j['series'].keys(), 'Missing mandatory EVDS or TurkStat series'
for k,s in j['series'].items():
    assert re.fullmatch(r'[A-Za-z0-9_-]+',k) and required<=s.keys(),k
    assert s['frecuencia'] in ('Diaria','Semanal','Mensual','Trimestral','D','W','M','Q'),k
    obs=s['observaciones']; assert len(obs)>0,k
    dates=[]
    for day,v in obs:
        d=dt.date.fromisoformat(day); assert d<=dt.date.today(),(k,day)
        assert type(v) in (float,int) and math.isfinite(v),k
        if s['frecuencia'] in ('M','Mensual'): assert d.day==1,k
        dates.append(d)
    assert dates==sorted(set(dates)),k
print(f'Validated {len(j["series"])} series; no empty or duplicate observations.')
