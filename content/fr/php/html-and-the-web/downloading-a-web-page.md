---
date: 2024-01-20 17:44:23.256679-07:00
description: "How to: Il n\u2019y a rien de plus simple que d'utiliser `file_get_contents`\
  \ pour attraper le contenu brut d'une page web ."
lastmod: '2024-04-05T21:53:59.362793-06:00'
model: gpt-4-1106-preview
summary: "Il n\u2019y a rien de plus simple que d'utiliser `file_get_contents` pour\
  \ attraper le contenu brut d'une page web ."
title: "T\xE9l\xE9chargement d'une page web"
weight: 42
---

## How to:
Il n’y a rien de plus simple que d'utiliser `file_get_contents` pour attraper le contenu brut d'une page web :

```PHP
<?php
$url = "http://example.com";
$content = file_get_contents($url);

if ($content !== false) {
    // Traitement du contenu
    echo $content;
} else {
    // Gérer l'erreur
    echo "Impossible de télécharger la page.";
}
?>
```

Sortie attendue : Le contenu HTML entier de http://example.com.

## Deep Dive
C'est vieux comme le web – PHP permet de télécharger du contenu depuis la naissance des fonctions de file system wrappers. Les alternatives incluent cURL, plus robuste et flexible :

```PHP
<?php
$curl = curl_init("http://example.com");
curl_setopt($curl, CURLOPT_RETURNTRANSFER, true);
$pageContent = curl_exec($curl);
if ($pageContent === false) {
    echo "Erreur cURL : " . curl_error($curl);
} else {
    echo $pageContent;
}
curl_close($curl);
?>
```

Avec cURL, vous avez plus de contrôle : gestion des en-têtes, des cookies, des délais d'attente, etc. Utilisez `file_get_contents` pour des cas simples, cURL pour la puissance et la précision. Dans les deux cas, pensez à la sécurité : nettoyage des données, validation des URL et gestion d'erreurs.

## See Also
Pour creuser, consultez la documentation officielle :

- PHP `file_get_contents`: [php.net/manual/fr/function.file-get-contents.php](https://www.php.net/manual/fr/function.file-get-contents.php)
- PHP cURL: [php.net/manual/fr/book.curl.php](https://www.php.net/manual/fr/book.curl.php)
- Bonnes pratiques de sécurité PHP: [php.net/manual/fr/security.php](https://www.php.net/manual/fr/security.php)
