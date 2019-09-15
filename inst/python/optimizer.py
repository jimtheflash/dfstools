import sys
from pydfs_lineup_optimizer import get_optimizer, Site, Sport, CSVLineupExporter

# read in the external arguments
input_string = sys.argv[1]
platform = sys.argv[2]
sport = sys.argv[3]
nlineups = sys.argv[4]
maxexp = sys.argv[5]
maxrepplyr = sys.argv[6]

# build the optimizer args
# Site
if platform == 'draftkings':
    SITE = Site.DRAFTKINGS
elif platform == 'fanduel':
    SITE = Site.FANDUEL
# Sport
if sport == 'nfl':
    SPORT = Sport.FOOTBALL
elif sport == 'nba':
    SPORT = Sport.BASKETBALL
    
# make optimizer
optimizer = get_optimizer(site = SITE, sport = SPORT)
optimizer.load_players_from_csv(input_string)
optimizer.set_max_repeating_players(int(maxrepplyr))

# export
exporter = CSVLineupExporter(optimizer.optimize(n=int(nlineups), max_exposure=float(maxexp)))
output_string = str(input_string[:input_string.rfind('/')+1]) + 'optimized.csv'
exporter.export(output_string)