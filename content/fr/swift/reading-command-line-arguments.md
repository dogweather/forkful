---
title:                "Swift: Lecture des arguments en ligne de commande"
simple_title:         "Lecture des arguments en ligne de commande"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Pourquoi

Si vous programmez en Swift, il est probable que vous utilisiez régulièrement la ligne de commande pour exécuter vos applications. Saviez-vous que vous pouvez également utiliser la ligne de commande pour lire des arguments dans votre code ? Cela peut être très utile pour personnaliser le comportement de votre application en fonction des entrées de l'utilisateur.

# Comment faire

Étape 1 : Ouvrez votre projet Xcode et créez une nouvelle classe dans votre dossier de code source.

Étape 2 : Dans cette classe, utilisez la méthode statique "main" pour récupérer les arguments de la ligne de commande. Cela se fait en utilisant la structure "CommandLine.arguments", qui stocke les arguments saisis par l'utilisateur lors de l'exécution de l'application.

Étape 3 : Vous pouvez maintenant utiliser ces arguments dans votre code pour effectuer différentes actions en fonction de leur valeur. Par exemple, si votre application doit s'exécuter en mode sombre, vous pouvez utiliser la commande "swift run MyApp --dark" pour récupérer l'argument "--dark" et changer l'apparence de votre application en conséquence.

````Swift
// Exemple de code pour récupérer et utiliser les arguments de la ligne de commande

class MyApp {

    static func main(_ arguments: [String]) {

        if arguments.contains("--dark") {
            // Changer l'apparence en mode sombre
        }

        if arguments.contains("--debug") {
            // Afficher les messages de débogage
        }

        // Autres actions en fonction des arguments
    }
}

MyApp.main(CommandLine.arguments) // Appel de la méthode avec les arguments
````

# Plongée en profondeur

Vous pouvez également utiliser la méthode "readLine" pour lire les entrées de l'utilisateur en cours d'exécution de l'application. Cette méthode prend en paramètre un message à afficher avant que l'utilisateur ne saisisse une valeur et renvoie cette valeur sous forme de chaîne de caractères.

````Swift
// Exemple d'utilisation de la méthode "readLine"

print("Entre ton nom d'utilisateur :")
if let username = readLine() {
    // Utilisation du nom d'utilisateur saisi
}
````

Vous pouvez également utiliser des bibliothèques externes pour vous aider à analyser les arguments de la ligne de commande de manière plus avancée, telles que "CommandLineKit" ou "SwiftCLI". N'hésitez pas à les explorer pour trouver la solution qui convient le mieux à vos besoins.

# Voir aussi

- [Documentation officielle de la structure CommandLine](https://developer.apple.com/documentation/foundation/commandline)
- [Exemple de projet utilisant la lecture d'arguments de la ligne de commande](https://github.com/hisaac/CommandLineArgsExample)
- [Bibliothèque externe CommandLineKit](https://github.com/jatoben/CommandLineKit)