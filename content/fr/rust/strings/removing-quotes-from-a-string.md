---
date: 2024-01-26 03:42:16.092336-07:00
description: "Supprimer les guillemets d'une cha\xEEne en Rust consiste \xE0 \xE9\
  liminer les caract\xE8res de guillemets superflus qui pourraient \xEAtre imbriqu\xE9\
  s autour de vos\u2026"
lastmod: '2024-02-25T18:49:54.283962-07:00'
model: gpt-4-0125-preview
summary: "Supprimer les guillemets d'une cha\xEEne en Rust consiste \xE0 \xE9liminer\
  \ les caract\xE8res de guillemets superflus qui pourraient \xEAtre imbriqu\xE9s\
  \ autour de vos\u2026"
title: "Retirer les guillemets d'une cha\xEEne"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Supprimer les guillemets d'une chaîne en Rust consiste à éliminer les caractères de guillemets superflus qui pourraient être imbriqués autour de vos données textuelles. Les programmeurs font cela lorsqu'ils ont besoin de nettoyer ou de normaliser des chaînes, peut-être après avoir analysé des données à partir d'un fichier, ou lorsqu'ils les préparent pour un autre format où les guillemets pourraient poser problème ou être redondants.

## Comment faire :

```Rust
fn remove_quotes(s: &str) -> String {
    s.trim_matches(|c| c == '\"' || c == '\'').to_string()
}

fn main() {
    let quoted_str = "\"Bonjour, Rustaceans !\"";
    let cleaned_str = remove_quotes(quoted_str);
    println!("{}", cleaned_str);
    // Sortie : Bonjour, Rustaceans !
}
```

Parfois, vous avez une chaîne avec des guillemets mixtes, comme ceci :

```Rust
fn main() {
    let mixed_quoted = "'Rust dit : \"Bonjour, Monde !\"'";
    let cleaned_str = remove_quotes(mixed_quoted);
    println!("{}", cleaned_str);
    // Sortie : Rust dit : "Bonjour, Monde !"
}
```

Ici, seuls les guillemets simples extérieurs sont supprimés.

## Plongée Profonde

Lors de la suppression de guillemets d'une chaîne, vous pourriez vous demander pourquoi il ne suffit pas simplement d'un `.replace("\"", "")`. Au début, le traitement du texte était moins standardisé et différents systèmes avaient différentes manières de stocker et de transmettre le texte, souvent avec une sorte de 'séquence d'échappement' pour les caractères spéciaux. La méthode `trim_matches` de Rust est plus polyvalente, vous permettant de spécifier plusieurs caractères à élaguer, et si vous souhaitez élaguer du début (préfixe), de la fin (suffixe), ou des deux côtés de la chaîne.

Il existe, bien sûr, des alternatives. Regex est la centrale de manipulation de chaînes, capable de faire correspondre des motifs complexes, et serait excessif pour juste supprimer les guillemets. Des bibliothèques comme `trim_in_place` pourraient offrir un élagage sur place sans la surcharge de création d'un nouvel objet `String`, ce qui pourrait être souhaitable pour des applications critiques en termes de performance.

Sous le capot, `trim_matches` itère en fait à travers les caractères de la chaîne des deux extrémités, vérifiant en fonction du motif fourni jusqu'à ce qu'un caractère non correspondant soit trouvé. C'est efficace pour ce qu'il fait, mais soyez toujours conscient qu'il travaille avec des valeurs scalaires Unicode. Si votre chaîne peut contenir des caractères Unicode multioctets, vous n'avez pas à vous soucier qu'elle les divise.

## Voir Aussi

- La documentation de Rust sur la manipulation de chaînes : https://doc.rust-lang.org/book/ch08-02-strings.html
- La crate `regex` pour des motifs complexes : https://crates.io/crates/regex
- Rust par l'exemple pour des scénarios de codage pratiques : https://doc.rust-lang.org/stable/rust-by-example/std/str.html
