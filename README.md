# Algorithmique_biclustering

## Projet 3 : biclustering algorithm
## Groupe 1 : Paul Mazet, Mathilda Rosset, Etienne Gaucher

### Description du projet

Les matrices binaires de grande dimension sont très présentes dans le domaine de la bioinformatique pour représenter des données génétiques. Le plus souvent, les lignes de la matrice correspondent à des gènes et les colonnes symbolisent des conditions expérimentales. La matrice binaire montre si un gène s'est exprimé dans une condition particulière. L'intérêt du biclustering est de trouver des *patterns* dans la matrice afin d'identifier des gènes qui se comportent de la même manière pour un sous-ensemble de conditions. Contrairement au clustering, le biclustering a la capacité de distinguer des *patterns* locaux, c'est-à-dire des gènes qui ont un comportement similaire seulement dans quelques conditions. Cependant, trouver l'ensemble des biclusters par une solution naïve est impossible, car la complexité est exponentielle. Des algorithmes récursifs, itératifs ou encore génétiques sont plus appropriés. Ainsi, nous avons implémenté 3 algorithmes différents :
- BiMax (Binary inclusion Maximal algorithm), un algorithme récursif introduit par Prelic et al.
- BiBit (Bit-Pattern Biclustering Algorithm), une méthode itérative proposée par Rodriguez-Baena et al.
- iBBiG (iterative binary bi-clustering of gene sets), un algorithmique heuristique basée sur une méthode génétique présentée par Daniel Gusenleitner et al.

### Description du package

Le package est divisé en plusieurs dossiers, dont les principaux sont :
- R :  scripts en langage R de BiMax et BiBit, scripts en R et C++ de iBBiG
- resultats : résultats des algorithmes sur des exemples définis et matrice binaire de LaMME
- simulation : scripts R qui permettent de réaliser des simulations pour vérifier la complexité de BiMax et BiBit
- annexes : ensemble des articles scientifiques sur lesquels nous nous sommes informés


### Import de notre package

Pour importer notre package, il suffit simplement de l'importer avec devtools grâce à la commande suivante : 
**devtools::install_github("matirosset/Algorithmique_biclustering")**
