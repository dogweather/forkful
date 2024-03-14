---
date: 2024-01-26 04:10:01.993126-07:00
description: "Utiliser un d\xE9bogueur, c'est comme se donner une vision aux rayons\
  \ X pour jeter un coup d'\u0153il \xE0 l'ex\xE9cution de votre code. Les programmeurs\
  \ le font pour\u2026"
lastmod: '2024-03-13T22:44:57.486607-06:00'
model: gpt-4-0125-preview
summary: "Utiliser un d\xE9bogueur, c'est comme se donner une vision aux rayons X\
  \ pour jeter un coup d'\u0153il \xE0 l'ex\xE9cution de votre code. Les programmeurs\
  \ le font pour\u2026"
title: "Utilisation d'un d\xE9bogueur"
---

{{< edit_this_page >}}

## Quoi et pourquoi ?

Utiliser un débogueur, c'est comme se donner une vision aux rayons X pour jeter un coup d'œil à l'exécution de votre code. Les programmeurs le font pour repérer les bugs, comprendre le flux du programme et s'assurer que leur code est aussi clair que possible. C'est comme avoir un ami qui vous indique exactement où vous avez trébuché.

## Comment faire :

Rust prend en charge divers débogueurs, mais un courant est `gdb` pour GNU/Linux ou `lldb` pour macOS. Vous pourriez également utiliser `rust-gdb` ou `rust-lldb` qui sont des enveloppes qui affichent joliment les valeurs Rust. Voici un aperçu :

```Rust
fn main() {
    let mut compteur = 0;
    for _ in 0..5 {
        compteur += 1;
        println!("Le compteur est à : {}", compteur);
    }
}
```

Pour déboguer cela, compilez avec des informations de débogage :

```shell
$ rustc -g counter.rs
```

Ensuite, exécutez-le dans `rust-gdb` :

```shell
$ rust-gdb counter
(gdb) break main
(gdb) run
(gdb) print compteur
$1 = 0
(gdb) continue
Le compteur est à : 1
(gdb) print compteur
$2 = 1
```

## Plongée en profondeur

Le débogage existe depuis *les anciens temps* des cartes perforées, et son évolution a été une bénédiction. Rust fournit ses propres outils avec des intégrations pour GDB et LLDB en raison de la nature de système de bas niveau du langage.

Les alternatives pour déboguer le code Rust incluent l'utilisation des environnements de développement intégrés (IDE) avec leurs débogueurs intégrés, que certains trouvent plus intuitifs. Parmi les plus populaires, on trouve CLion avec le plugin Rust ou Visual Studio Code avec l'extension Rust.

Pour ce qui est de l'implémentation, Rust génère des symboles de débogage que ces débogueurs comprennent, ce qui est essentiel pour parcourir le code, placer des points d'arrêt et inspecter les variables sans perdre la tête.

## Voir également

- Le livre Rust sur le débogage : https://doc.rust-lang.org/book/ch09-02-recoverable-errors-with-result.html#guidelines-for-error-handling
- La perspective de Rust By Example sur les erreurs et le débogage : https://doc.rust-lang.org/rust-by-example/error.html
- Le serveur de langage Rust (RLS) qui alimente l'extension Rust de VS Code : https://github.com/rust-lang/rls
- Déboguer Rust avec Visual Studio Code : https://marketplace.visualstudio.com/items?itemName=rust-lang.rust
