// wordle.js | Copyright (c) 2022 Tristan Burke | www.tristan-burke.com

// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:

// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.

// Constants
const green = "#2A7128";
const yellow = "#E9E94F";
const grey = "#B3B3B3";
const default_color = "#EFEFEF";

const wordSet = new Set(["delta","slade","scarf","raise","hanky","manga","mused","tiara","keyed","hamza","swoon","metis","azide","block","width","chump","palsy","emery","longs","salvo","grant","sheep","ninth","onion","month","lying","flaky","blake","pecan","flyer","drago","sewer","tenor","chile","hilly","screw","spray","chess","timer","nonce","kilim","brunt","drool","swore","harry","polka","penna","godly","spelt","atoll","divan","halve","valor","rated","pilar","jimmy","layer","uncle","judge","resin","gland","lager","quill","rainy","sedan","tying","polio","cabot","white","first","willy","blast","chart","input","tical","wiser","faust","ronde","masse","camel","risky","clerk","panic","under","theft","halal","mount","lymph","apron","freed","outer","found","death","gravy","hunky","hitch","vying","rumen","upper","crack","havoc","munch","drill","laura","lotto","putty","tetra","spoof","pique","paste","mimeo","brief","probe","nomic","oxide","curry","frame","robot","chute","hiker","seedy","smite","snoop","tally","koala","rally","urine","scant","hoard","beast","glide","manna","bylaw","flank","snore","beach","hiper","minot","power","liver","lunar","ought","serum","risen","henna","quack","armor","laden","zesty","chord","padre","rance","asker","ready","truck","dirge","gemma","felon","tipsy","globe","boost","razer","grebe","flair","rowdy","blair","feast","benny","evade","tepid","final","inane","mufti","beset","udell","zante","lowly","snort","tenet","anion","crept","axiom","budge","showy","conic","swine","reddy","serve","chips","cramp","enter","shiny","stiff","patty","epoch","these","emcee","lindo","scrap","tempt","mural","nexus","luger","valve","teach","dried","crown","messy","grice","until","grape","sassy","inlay","empty","pinto","swamp","inter","prana","argue","bebop","steal","passe","drape","hazen","balmy","overt","nitty","bison","sooty","mimic","fitch","coast","stoop","close","state","savin","colin","pardo","drown","evens","birch","witty","furry","hooch","trust","bound","kodak","hedge","torus","sixty","chaff","unfit","debit","fiend","flood","piano","table","sport","matte","rhine","truer","blanc","stunt","peggy","banal","cadre","humid","larva","tacky","forge","levee","lined","creed","sunny","stole","being","cheat","scorn","piezo","scone","tanga","gator","spook","kappa","julio","river","grave","pyrex","smile","debut","therm","heavy","taxed","gully","benet","relic","fishy","baggy","zorro","clone","stork","sugar","caged","busby","fader","voter","mitra","chico","saver","melon","larch","pubic","music","conus","capes","plain","comet","worth","motel","await","stung","twain","hobby","glare","unset","torch","sappy","rogue","mower","lousy","moldy","plaza","fried","tonal","cache","hokey","bandy","litas","feist","rupee","bread","braid","barry","phage","caste","viper","mavis","thump","libel","rummy","scart","diner","waive","chard","thorp","revel","needy","sheer","glade","taker","anise","skies","kraft","broth","batch","auger","mango","loony","salty","truce","taint","leaky","claim","bravo","attic","runny","huron","remix","dummy","marge","askew","fatal","brake","spike","maple","hasta","lazar","asset","delve","semen","plead","craft","total","ropes","albee","murky","ashes","pulse","annoy","passo","fable","valid","dazed","snail","bogey","siege","tween","aback","dodgy","damme","whiff","aland","usage","giver","ecole","dingo","quake","award","lemma","parry","prion","loser","paolo","scoop","quilt","verge","wasnt","raver","china","bowel","sloop","north","horst","ender","plied","aviso","azure","noose","crock","carat","squid","winer","hurry","haven","witch","slung","issue","ghost","elect","plumb","cynic","jewel","toast","sever","misty","thank","arise","wakes","cross","drank","bosom","occur","revue","gourd","creep","giddy","kiosk","snout","dance","fecal","litho","crude","pilot","jerry","guise","optic","drake","clark","latex","zoned","bourg","shone","psych","angle","truss","greed","cleft","mince","pride","grand","mange","bushy","macaw","house","daisy","thorn","fraud","semis","spore","bland","laced","renal","blues","lewis","rocky","jager","panty","minor","logos","perky","plaid","plage","rondo","derma","infer","moyen","lurid","hotly","where","today","altar","toric","bucky","butte","arrow","weave","fugue","cruel","fluke","sizes","flask","snafu","jours","sieve","times","skiff","lemon","smash","woman","saggy","topic","whisk","medic","smear","jenny","leafy","spode","chest","savoy","peres","conga","buddy","stain","camus","wafer","salle","frith","agape","heaps","plier","knead","model","cardo","could","draft","akasa","laine","think","queen","decay","react","tried","haste","scout","smell","brink","rabid","spurt","gauge","bagel","ethyl","throb","stair","clash","group","lanas","usury","smoot","stead","cruse","blaze","fresh","conch","pants","midst","titer","sitar","tania","robin","whack","zaman","forth","login","folly","durst","slows","crime","hover","shady","spree","cased","alway","proto","surly","bruin","stove","shove","privy","clave","bogus","josie","comfy","offer","broke","satin","molly","juror","yucca","meson","leper","feria","alder","amour","criss","knack","vista","donor","kerry","turbo","meter","setup","saved","penal","aging","dowry","owner","lysis","corgi","aloof","horse","widen","organ","erupt","brill","zippy","carol","chirp","radar","flite","horde","sleep","bleak","whose","matin","glyph","shift","boner","often","stuck","phone","clump","chaps","medal","relay","groot","hater","shall","groan","lucky","doing","bizet","prawn","lough","tasty","viral","ruddy","squaw","stock","fungi","woven","cabal","edema","canal","codon","stale","nanny","salad","locus","swirl","shine","climb","rigor","didst","speed","thief","forme","scape","blunt","boxer","actor","while","gaunt","nitro","conto","petit","frost","brook","friar","cadet","agent","imply","slimy","quits","crepe","alike","quash","eight","vomit","derby","floor","silly","nifty","lumen","morel","below","sword","click","tunic","yacht","tabor","taste","slate","rayon","libra","gorge","skier","exist","canon","loopy","pombo","alley","aspen","berth","query","value","belle","mocha","coach","meant","wince","skein","might","tango","fluff","pygmy","bolus","flora","built","pater","freak","mille","roque","yahoo","cutie","dixit","bowie","alive","media","hired","crier","sided","sault","avail","water","spank","enjoy","cubic","faded","tuber","chalk","windy","llama","picky","adieu","weber","tower","purdy","baldy","adorn","sober","situs","spare","helio","genie","swarm","heave","twine","elves","recap","sauna","blame","kiddy","servo","oriel","sally","silva","loris","twang","green","didnt","wreak","blitz","funny","cater","abbot","prune","banjo","palma","liven","piece","ensue","ahead","scaly","bathe","slash","exile","gumbo","cuddy","nudge","lower","start","ambit","punto","scale","tired","strum","femur","waste","smart","tract","hough","dress","glace","hunch","peace","audio","train","leach","redox","tamer","dealt","prove","ingot","trace","sutra","churn","webby","chair","pinch","duchy","cress","doubt","pedro","flesh","short","knelt","speak","large","dying","polar","sadly","point","noble","noise","payee","talon","rider","rover","twice","flame","malik","spark","bough","coder","rheum","manic","eerie","sizer","prime","saucy","bumpy","court","deity","thong","spell","later","colic","treat","eaves","esker","porky","alien","score","druid","sulla","patch","virus","touch","sharp","groff","hubby","finch","snell","tenth","finer","corse","wrong","swell","brock","yearn","vixen","theme","japan","vodka","riyal","solar","obese","swank","fives","muang","poppa","cumin","pizza","thigh","after","ivory","flown","cline","grief","sweet","clips","clock","perry","stalk","jerky","waver","earth","three","rufus","quart","surah","breve","shrug","arent","pivot","title","rigid","dandy","lever","calor","singh","bunny","super","steel","guess","snook","corny","goody","widow","penis","waker","silty","ghoul","arson","faced","peril","prine","kraut","darby","flung","justo","based","piggy","schuh","larry","audit","sisal","merit","idiom","posse","sleet","break","villa","sense","deuce","shoji","savor","fetal","scran","ditto","gyrus","merry","spicy","elite","bobby","taxis","folio","graze","grate","erase","forte","jihad","devil","racer","crank","enrol","sheik","jakes","scrip","stage","cloud","reese","evoke","chain","sandy","chick","buyer","playa","jaded","brand","lisle","rowan","puree","civil","verse","ozone","hello","faint","maris","tummy","radii","pause","krona","roomy","booty","gross","roper","waist","beano","human","fluor","timor","front","bless","boron","lyric","toxin","peony","solid","flint","chevy","quest","hasty","blurb","royal","helix","pacer","ganga","round","cobra","etude","trend","honey","melee","grain","betty","chaos","amber","neigh","verve","pupil","alamo","laird","shred","buggy","decoy","spunk","bijou","strap","quell","ended","areal","uncut","young","mania","regal","pylon","taboo","genus","fuzzy","fossa","lusty","genre","mayor","pound","lotus","embed","takin","steer","chomp","daddy","param","tutti","blown","worst","pitch","broad","lodge","arose","bitty","utile","timed","scare","mover","frown","grown","sitio","karma","gauze","dozen","smack","rebut","vexed","homey","prose","defer","synch","cleat","gecko","medio","poser","motto","poise","punta","smith","cheek","cable","foyer","kabel","trout","store","carry","scamp","trump","whore","stump","jazzy","sheaf","hyena","crisp","chunk","snark","duper","vivid","wired","amino","aught","havel","rebel","apple","cloth","shark","calyx","basin","anger","banca","taiga","spong","steen","reeve","steam","aster","phony","wield","awful","oasis","sniff","drink","align","shock","beech","strut","vouch","album","troll","fiber","noisy","waltz","comic","cough","lapel","reach","payor","visto","brawl","vital","easel","tammy","tater","shunt","clout","triad","lupus","clint","shaul","tryst","slice","acorn","denim","borne","trade","recon","anvil","glean","incur","genet","tread","magma","unite","brant","butyl","tacit","assis","nymph","datum","waged","taunt","smyth","vogue","prius","shawl","strip","fifty","march","aorta","basal","mason","avian","fence","kelly","ville","bogle","agate","twill","oiled","bally","vocal","bases","noted","gloss","lumpy","beryl","towel","largo","catan","prowl","ruble","fluid","maize","opium","spawn","drawn","clasp","dover","berne","yerba","suing","tubal","sneak","saint","gable","liber","amine","slump","other","mezzo","mitre","funds","quail","wally","howdy","claro","trunk","unmet","seine","pombe","skate","flirt","corps","breck","roast","level","oliva","fussy","clear","greet","spoke","rosin","fleet","elude","annex","smoky","troop","brisk","mater","ethos","adobe","whats","macao","spink","petty","grail","nurse","paper","begun","xenon","spitz","glint","never","sonny","lilac","sling","salix","irate","fired","aside","nancy","knock","bream","daily","alter","clank","crypt","whoop","carbo","berry","icing","scalp","cello","rabbi","topaz","garth","abate","breed","chili","facet","video","ethic","crust","pablo","messe","sound","charm","baron","tease","class","barth","glory","carer","coral","night","varna","culpa","rogan","ortho","caper","strom","olden","pokey","jumbo","gouge","feral","flour","matra","reuse","manly","board","nicky","weary","fryer","fairy","firth","filth","hardy","aloud","igloo","utter","again","reset","frist","wedge","alert","reina","hoist","pious","stray","needs","creel","phare","scrub","swami","gripe","stang","piles","slurp","choir","about","snack","press","guest","covey","coker","sides","radon","knoll","bayou","splat","merch","mores","motif","adult","great","smoke","taupo","curio","flack","trash","retry","affix","tabby","kebab","wages","trice","aloft","cargo","ringe","gummy","dunne","moors","madly","crass","miler","wharf","avert","undue","roost","error","paint","clean","macro","chuck","child","daffy","dough","belly","sucre","sence","lynch","spent","butch","quirk","scent","vireo","testa","gules","stone","manta","haunt","swung","linea","conte","lorry","goran","abyss","billy","chewy","hymen","inlet","anode","piper","liege","shook","rebar","leapt","sissy","woody","gabby","grout","grove","stave","awake","leech","write","leger","amiss","inner","naive","moody","demos","cheer","swear","angel","fleer","vinny","hazel","blend","place","mound","aller","allay","hairy","erode","order","slain","pansy","equal","drier","stark","herne","glove","giant","hogan","minus","terry","aliso","filmy","alloy","totem","monte","chief","mossy","craps","benzo","fatty","amaze","focus","circa","trite","entry","worse","arena","depot","chore","along","manny","rival","arbor","spill","ponce","poked","snaps","massa","bulky","boned","skill","bevel","spoil","fosse","grits","honda","sorry","petal","noddy","terse","blade","blush","rhino","quick","quiet","nylon","brash","shoal","stall","foray","fetus","eared","spice","funky","drive","slope","heart","watch","flick","shard","exert","motor","boots","joker","enema","trier","allan","quote","whole","purse","baked","dowel","quint","rower","bulge","stile","agora","shore","purge","metra","cored","titty","graft","magic","glued","image","waugh","voice","stink","canna","compo","moral","sigma","proof","hefty","byway","samba","wrath","swath","going","olive","mouse","colon","spina","hunks","selah","above","mould","spend","taper","titre","soles","bugle","rouse","tight","alias","hoped","torso","phono","indic","sting","merge","blick","crumb","groom","polos","bluff","wound","steed","mirza","rifle","machi","slush","ideal","patio","shade","lance","cried","orang","niche","cabin","miser","apply","couch","pekin","piety","twink","visor","mauve","thing","mouth","gusto","shell","style","vowel","taffy","feces","bigot","infra","light","sepia","guile","paisa","angst","boden","laney","perle","limbo","locum","admit","allow","stays","biota","recur","sloan","panda","ledge","crunk","price","those","bingo","belay","spout","every","clima","check","kroon","jenna","shoot","craze","sneer","chang","banco","golem","scene","wahoo","toned","punch","third","stuff","color","mourn","owing","atria","reply","clamp","idiot","marry","facia","chien","vigil","debby","bliss","penny","ranch","index","maxim","brace","slack","droit","wiper","spasm","sized","quark","riser","hoary","thane","diddy","hondo","older","vicar","shaky","skunk","barra","cocky","amply","anime","mains","dwarf","boast","tilde","badge","razor","money","leave","tribe","squat","stein","thema","there","poppy","newly","tuned","tulle","inert","cairn","stull","filet","lurch","loran","unity","slick","batty","drunk","islet","whorl","least","worry","scoot","squad","among","hippo","ariel","mucus","loyal","queer","union","fancy","cling","papal","ultra","missy","gnome","burke","smirk","pouch","repel","crawl","inept","assay","grind","milla","hyper","given","duvet","shaft","movie","tramp","alone","trick","armed","mercy","heron","their","plant","aware","ching","stand","fling","dunst","mulch","zloty","cotta","frank","swing","caddy","gavel","boyer","mushy","paver","suede","jacko","hanna","staff","fated","siren","adore","bleed","crate","suite","chaka","laver","shree","bacon","ionic","stood","saute","vapor","redux","skull","dodge","idler","costa","abuse","minty","teeth","label","bonus","nadir","radio","retin","burly","puffy","youth","tithe","spade","morse","gamma","tawny","kneel","otter","cease","gaudy","cones","broom","favor","chock","atlas","blink","lover","essay","mummy","catch","ryder","blood","raven","anima","sedge","linen","bloke","mumps","poker","laugh","tiled","trike","steep","dixie","prone","spiel","llano","septa","south","shake","elder","hippy","antes","psalm","astor","vault","aisle","spiny","erect","shrub","shrew","tutor","midge","flute","still","mines","foamy","thyme","levin","which","guide","hence","straw","crore","stoke","gauss","cover","adapt","equip","candy","slang","navel","tumor","gusty","abbas","verso","marsh","humor","shape","sweep","banda","snipe","glial","chase","umbra","metal","abort","grime","usual","scour","doggy","hasan","stent","black","torah","farce","ovary","onset","props","basta","civic","blank","range","tough","spied","imago","filly","sworn","truth","parle","match","digit","cider","grist","valet","molar","footy","unify","liner","prism","tongs","tweak","sleek","latch","dwell","girth","dusky","meaty","truly","grade","puppy","islay","layne","kelty","avast","agony","abide","excel","coroa","storm","muddy","legal","clung","plate","tiger","vinyl","maria","skirt","clive","gruff","sable","pluck","rhein","blind","whale","quite","patel","quoth","brain","false","salon","potty","merle","posey","natal","clary","dimer","carve","arles","coupe","viola","adept","tubby","chine","seton","monad","ladle","dense","vague","xenia","tangy","upset","kudos","loose","rotor","ocean","booby","pleat","ingle","lated","dirty","dobra","capel","axion","ember","grace","cheap","creat","hight","parma","cloak","sturt","stamp","avoid","wacky","mirth","ravel","cedar","birth","extra","labor","comes","shank","ample","gains","creek","clift","adder","prior","crazy","lathe","birdy","lunch","skeet","neath","moose","sixth","franc","arjun","rodeo","gamut","chime","shyly","party","khaki","peter","scope","tardy","weigh","throw","blimp","speer","stine","exact","bafta","quota","spiro","soapy","alban","barge","lasso","buffy","champ","rerun","hoyle","spire","shack","handy","algae","enact","lofty","works","inset","krone","ridge","bidet","hutch","husky","egret","sloth","goose","piled","serra","taken","timid","twist","demon","gypsy","apart","juice","rough","livid","scrum","pooch","tibet","sonar","shire","graph","bight","downy","gulch","shown","diary","daman","clove","envoy","degas","ralph","pesky","happy","pence","beefy","aleph","curie","shelf","diver","local","winch","spine","fiery","darts","orbit","slime","track","froze","bursa","amend","brass","swipe","plume","space","knife","pinky","tidal","wench","grass","taupe","abode","evict","small","fault","swiss","gault","chill","slide","flock","tooth","horny","drone","honor","ankle","aegis","heald","oddly","nodal","bring","basic","arche","ester","ebony","paddy","acker","snuck","rouge","smelt","snide","venue","delft","joshi","renin","bundy","bower","steak","satan","force","paced","hamel","rater","tabla","milky","quant","beige","acton","ether","alpha","yeast","drain","whine","holla","zonal","baker","gated","myron","forum","synod","broil","liang","crimp","goods","array","nutty","radix","lolly","yummy","axial","coram","crave","morin","spate","silex","huffy","mixed","brush","ghazi","aptly","fifth","world","sheng","wheat","thumb","freer","aphid","wager","aural","labia","slant","waved","pixie","lohan","adopt","vigor","sauce","sully","senna","laity","bongo","swept","thats","ferry","burst","thine","trawl","bowls","baton","ounce","comer","focal","octet","eater","fudge","amide","spite","story","bride","route","learn","porch","bilbo","sheet","afoot","share","flake","relax","selva","sperm","fully","shale","liter","steno","perch","tonga","shirt","erika","lived","croft","jetty","youve","rabat","oxbow","hinge","naked","ratio","bilge","ginny","shear","tilly","deter","frail","penta","cubby","vowed","cheng","riley","rinse","tuner","wigan","study","taxon","groin","glebe","dread","shalt","scuff","hotel","traps","booze","agile","kinky","walsh","decry","cocoa","alarm","alibi","stool","event","guard","thick","odeon","porto","helly","rumor","kylix","pubes","prick","sweat","aroma","stony","amuse","decal","stick","eject","rusty","sider","chasm","laser","chant","teens","comma","hertz","harem","beaux","stoic","micro","toxic","badly","dogma","maker","ganja","flash","froth","reedy","curse","refer","grasp","acute","hatch","grill","cliff","buxom","itchy","dildo","tommy","edged","nasty","forty","tarot","solon","joint","diode","shout","payer","balsa","cyrus","flush","would","ditch","dwelt","rivet","lucid","count","fight","actin","right","chose","cycle","wreck","teeny","stern","photo","smock","nasal","suave","beard","posit","adage","manor","renew","crone","glass","opera","yours","corky","covet","hansa","rapid","omega","naval","build","holly","surge","nomad","peeve","kitty","unary","swift","snape","goofy","atmos","brine","float","field","bribe","slave","phase","batik","guilt","lingo","abbey","caret","crane","basil","soggy","trait","bande","drama","dimly","pager","drove","serif","pearl","ditty","crook","casco","scion","stade","tonic","booth","shure","begin","cacao","seven","spoon","savvy","sheen","bully","canoe","edict","antal","elbow","matta","known","sammy","cacti","alkyl","rohan","weird","flare","homer","ethel","swain","jolly","sonic","dolor","flier","habit","chino","pasty","bloat","dolly","fetch","mamma","clack","tempo","ligne","wheel","ulcer","silky","urban","limes","cleve","mommy","tapas","yield","eaten","graff","clown","kudzu","randy","hound","junta","smote","spool","lease","fever","peach","trine","leash","shave","moron","tense","cheep","brier","speck","crick","turns","curvy","sayer","tasco","expel","syrup","miner","floss","pussy","fruit","ascii","bench","donna","miter","ruler","rebus","panel","welsh","alist","swoop","fleck","cigar","delay","trail","harsh","crain","evert","heath","grunt","porta","cause","golly","voile","faith","brick","stout","pasha","shame","codex","brown","realm","sinus","stare","tweed","allot","snare","apnea","reins","swash","seize","griff","jeans","norma","glaze","spear","slept","mambo","bitch","kayak","basso","jerez","payed","novel","repay","spilt","juicy","enemy","canto","links","umber","gleam","weeps","growl","serge","agree","prize","beret","tulip","filer","doria","polis","eager","roach","limit","brute","hakim","parse","cream","usher","snowy","swish","major","elegy","plush","prima","hurst","nerve","ascot","emmet","crash","brood","gluck","maybe","algal","frosh","negro","bosch","stack","marco","sahib","pedal","guild","lapse","cisco","lemur","awoke","stash","bloom","natty","since","piney","moray","early","plane","carte","binge","nappy","girly","parka","fixed","rhyme","bonny","motte","theta","pagan","dinar","burnt","reign","wight","guppy","wrote","maint","twirl","dutch","jiffy","roger","brent","trial","hydro","amman","canny","whiny","scala","dizzy","depth","toner","token","wagon","cajun","eagle","threw","plank","tripe","moist","piped","dingy","curve","curly","cameo","fused","bowed","snuff","tinge","vulva","henry","foggy","surat","crest","lobby","saber","nosed","notch","matti","madam","split","sight","tales","irene","quasi","added","whirl","venom","logic","myrrh","modal","karst","visit","dozer","basis","legit","dotty","drift","fixer","amity","prank","plies","forgo","angry","remit","dream","proxy","queue","trove","argon","gloom","burgh","scary","crush","niece","briar","specs","bezel","faery","campo","borer","crump","mensa","livre","stomp","salma","rural","dinky","tibia","cinch","morph","print","zebra","heady","derry","kiley","stint","mixer","brave","stake","choke","dairy","wrist","brett","joule","guava","musty","jelly","hives","fiche","crowd","tarry","bunch","irony","salve","dusty","awash","prong","solve","snake","plump"]);
const wordArr = Array.from(wordSet);
const allLetters = "qwertyuiopasdfghjklzxcvbnm".split("");

const specialWords = ["penis", "cummm", "gayyy"];


// Game Variables
var row = 0;
var col = 0;

var word = generateWord();
var currGuess = [null, null, null, null, null];
var gameOver = false;

window.onload = (event) => {
  generateWord();
};

//Set Listener
document.addEventListener('keydown', function(event) {

    // If entry is a character
    if (event.keyCode >= 65 && event.keyCode <= 90) {
        // Check if there is space to add character
        if (col < 5 && row < 5 && !gameOver) {
            var character = event.key;
            // Set HTML and current guess variable
            document.getElementById('tile-' + row + col).innerHTML = event.key;
            currGuess[col] = event.key;

            col++;
        }
    }


    // If entry is delete key
    if (event.keyCode == 8 || event.keyCode == 46) {
        // Check if there are characters to delete
        if (col > 0 && !gameOver) {
            col--;

            // Set HTML and current guess variable
            document.getElementById('tile-' + row + col).innerHTML = "";
            currGuess[col] = null;

        }
    }

    // If entry is enter key
    if (event.keyCode == 13) {
        event.preventDefault();

        if (gameOver) {
            new_game()
        }

        // Check if full word entered
        if (col == 5) {
            // Check if valid guess
            var guess = currGuess.join("");
            console.log(guess);
            if (wordSet.has(guess) || specialWords.includes(guess)) {

                // Evaluate the current guess at the current row
                evaluateGuess();


                row++;
                col = 0;

                // Check if Last guess
                if (row == 5) {
                    gameOver = true;
                }

            }
        }
    }
});


function generateWord() {
    var randomIndex = Math.floor(Math.random() * (wordArr.length + 1));
    var newWord = wordArr[randomIndex];
    console.log(newWord);
    return newWord;
}

function evaluateGuess() {
    // First check if game over
    if (currGuess.join("") == word) {
        endGame();
    }
    // Iterate through characters of current guess
    for (var i = 0; i < 5; i++) {
        if (currGuess[i] == word.charAt(i)) {
            document.getElementById('tile-' + row + i).style.backgroundColor = green;
            document.getElementById('keyboard-' + currGuess[i]).style.backgroundColor = green;
        } else if (word.includes(currGuess[i])) {
            document.getElementById('tile-' + row + i).style.backgroundColor = yellow;
            document.getElementById('keyboard-' + currGuess[i]).style.backgroundColor = yellow;
        } else {
            document.getElementById('tile-' + row + i).style.backgroundColor = grey;
            document.getElementById('keyboard-' + currGuess[i]).style.backgroundColor = grey;
        }
    }
}

function endGame() {
    gameOver = true;
}

function new_game() {
    console.log("New Game...");

    // Reset Variables
    row = 0;
    col = 0;
    word = generateWord();
    currGuess = [null, null, null, null, null];
    gameOver = false;

    // Clear coloring and grid
    for (var i = 0; i < 5; i++) {
        for (var j = 0; j < 5; j++) {
            document.getElementById('tile-' + i + j).innerHTML = "";
            document.getElementById('tile-' + i + j).style.backgroundColor = default_color;
        }
    }
    for (var i = 0; i < allLetters.length; i++) {
        document.getElementById('keyboard-' + allLetters[i]).style.backgroundColor = default_color;      
    }
}

function special_game() {
    console.log("New Game...");

    // Reset Variables
    row = 0;
    col = 0;
    word = specialWords[Math.floor(Math.random() * (specialWords.length + 1))];
    if (word == undefined) {
        word = "farts";
    }
    currGuess = [null, null, null, null, null];
    gameOver = false;

    console.log(word);

    // Clear coloring and grid
    for (var i = 0; i < 5; i++) {
        for (var j = 0; j < 5; j++) {
            document.getElementById('tile-' + i + j).innerHTML = "";
            document.getElementById('tile-' + i + j).style.backgroundColor = default_color;
        }
    }
    for (var i = 0; i < allLetters.length; i++) {
        document.getElementById('keyboard-' + allLetters[i]).style.backgroundColor = default_color;      
    }
}


