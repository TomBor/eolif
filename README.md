# Éolif : 🌬 ️carte de prévision de vents en gif
Pour un jour j, les prévisions de vents heure par heure pour le lendemain (6h j+1 à 6h j+2) sont téléchargées.
Une carte par heure sous forme de lignes de courant (*streamlines*) est générée.
Les 24 cartes sont rassemblées dans un gif. Le résultat est publié sur twitter : @EoleGif


## Sources
Données de vent : Météo France, [base Arome](https://donneespubliques.meteofrance.fr/?fond=produit&id_produit=131&id_rubrique=51) (modèle atmosphérique à aire limitée à haute résolution).
Fond de carte : [Natural Earth](https://www.naturalearthdata.com/)


## Remerciement
À [pluiff](https://github.com/mtmx/pluiff) de [MTMX](https://mtmx.github.io/) pour l'inspiration initiale.
Au package {[metR](https://github.com/eliocamp/metR)} d'Elio Campitelli qui permet la création des streamlines.
