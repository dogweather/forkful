---
title:    "Rust: Commencer un nouveau projet"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Pourquoi

Vous êtes peut-être excité à l'idée de démarrer un nouveau projet en Rust, mais vous vous demandez peut-être pourquoi vous devriez le faire. En plus d'être un langage de programmation moderne et performant, Rust est également réputé pour sa sécurité et sa fiabilité. De plus, sa communauté active et son écosystème en constante croissance en font un choix solide pour vos projets.

## Comment faire

Pour commencer un nouveau projet en Rust, vous aurez besoin de télécharger et d'installer le compilateur Rust ainsi que Cargo, le gestionnaire de paquets de Rust. Une fois cela fait, vous pouvez créer un nouveau projet en exécutant la commande `cargo new <nom du projet>` dans votre terminal. Cette commande va créer un dossier contenant tous les fichiers nécessaires pour votre projet.

Voici un exemple de code en Rust pour imprimer "Bonjour le monde !" dans la console :

```rust
fn main() {
    println!("Bonjour le monde !");
}
```

Après avoir enregistré ce code dans un fichier `main.rs`, vous pouvez le compiler en utilisant la commande `cargo run`. Vous devriez alors voir le texte "Bonjour le monde !" imprimé dans votre terminal.

## Plongée en profondeur

Il y a beaucoup de choses à découvrir lorsque l'on commence un nouveau projet en Rust. Vous pouvez créer des projets en utilisant différentes configurations, utiliser des macros pour automatiser des tâches répétitives, et même incorporer du code C ou C++ dans votre projet en utilisant les fonctionnalités de Rust telles que "unsafe". N'hésitez pas à explorer et à expérimenter avec la documentation officielle de Rust et les nombreuses ressources communautaires disponibles en ligne.

## Voir aussi

- [Site officiel de Rust](https://www.rust-lang.org/fr)
- [Documentation de Rust](https://doc.rust-lang.org/fr/)
- [Exemples de projets en Rust](https://github.com/rust-lang-learn/examples)
- [Community Discord de Rust en français](https://discord.gg/EDqanuMz)