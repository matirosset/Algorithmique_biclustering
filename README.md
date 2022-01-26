# Algorithmique_biclustering

## Projet 3 : biclustering algorithm
## Groupe 1 : Paul Mazet, Mathilda Rosset, Etienne Gaucher

### Description du projet

Les matrices binaires de grande dimension sont très présentes dans le domaine de la bioinformatique pour représenter des données génétiques. Le plus souvent, les lignes de la matrice correspondent à des gènes et les colonnes symbolisent des conditions expérimentales. La matrice binaire montre si un gène s'est exprimé dans une condition particulière. L'intérêt du biclustering est de trouver des "patterns" dans la matrice afin d'identifier des gènes qui se comportent de la même manière pour un sous-ensemble de conditions. Contrairement au clustering, le biclustering a la capacité de distinguer des "patterns" locaux, c'est-à-dire des gènes qui ont un comportement similaire seulement dans quelques conditions. Cependant, trouver l'ensemble des biclusters par une solution naïve est impossible, car la complexité est exponentielle. Des algorithmes récursifs, itératifs ou encore génétiques sont plus appropriés. Ainsi, nous avons implémenté 3 algorithmes différents :
- BiMax (Binary inclusion Maximal algorithm), un algorithme récursif introduit par Prelic et al.
- Bibit (Bit-Pattern Biclustering Algorithm), une méthode itérative introduite par Rodriguez-Baena et al.
- iBBiG (iterative binary bi-clustering of gene sets), un algorithmique heuristique basée sur une méthode génétique introduit par Daniel Gusenleitner et al.

### Description du package

Le package est divisé en plusieurs dossiers, dont les principaux sont :
- fd
