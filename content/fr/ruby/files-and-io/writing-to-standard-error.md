---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:15.890972-07:00
description: "Comment faire : La biblioth\xE8que standard de Ruby offre une mani\xE8\
  re simple d'\xE9crire sur stderr en utilisant `$stderr` ou `STDERR`. Vous n'avez\
  \ pas besoin\u2026"
lastmod: '2024-03-13T22:44:58.438326-06:00'
model: gpt-4-0125-preview
summary: "La biblioth\xE8que standard de Ruby offre une mani\xE8re simple d'\xE9crire\
  \ sur stderr en utilisant `$stderr` ou `STDERR`."
title: "\xC9crire sur l'erreur standard"
weight: 25
---

## Comment faire :
La bibliothèque standard de Ruby offre une manière simple d'écrire sur stderr en utilisant `$stderr` ou `STDERR`. Vous n'avez pas besoin de bibliothèques tierces pour cette opération basique.

### Écrire un message simple sur stderr :
```ruby
$stderr.puts "Erreur: Fichier non trouvé."
# Ou de manière équivalente
STDERR.puts "Erreur: Fichier non trouvé."
```
Exemple de sortie (sur stderr) :
```
Erreur: Fichier non trouvé.
```

### Rediriger stderr vers un fichier :
```ruby
File.open('error.log', 'w') do |file|
  STDERR.reopen(file)
  STDERR.puts "Échec de l'ouverture de la configuration."
end
```
Ce morceau de code redirige stderr vers un fichier nommé `error.log`, et toutes les erreurs écrites subséquentes seront sorties là jusqu'à ce que le programme réinitialise la redirection de stderr ou se termine.

### Utiliser stderr avec la gestion des exceptions :
```ruby
begin
  # Simulant une opération qui pourrait échouer, par exemple, ouvrir un fichier
  File.open('fichier_inexistant.txt')
rescue Exception => e
  STDERR.puts "Une exception s'est produite : #{e.message}"
end
```
Exemple de sortie (sur stderr) :
```
Une exception s'est produite : Aucun fichier ou dossier de ce type @ rb_sysopen - fichier_inexistant.txt
```

Bien que les méthodes intégrées de Ruby pour écrire sur stderr suffisent pour de nombreuses applications, pour des besoins de journalisation plus complexes, vous pourriez envisager la bibliothèque standard `logger` ou des gems externes comme `Log4r`. Ces derniers fournissent des mécanismes de journalisation configurables, incluant les niveaux de sévérité, le formatage, et la capacité d'écrire sur diverses sorties, incluant des fichiers, des courriels, et plus encore.
