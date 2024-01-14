---
title:    "Arduino: Recherche et remplacement de texte"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Pourquoi

Vous êtes-vous déjà retrouvé(e) à devoir remplacer du texte dans votre code Arduino ? Peut-être que vous voulez changer un mot ou une variable qui se répète plusieurs fois. Au lieu de faire ces modifications manuellement et risquer de faire des erreurs, il est plus efficace de faire une recherche et un remplacement de texte. Cela vous permettra de gagner du temps et d'éviter les erreurs dans votre code.

## Comment faire

Pour effectuer une recherche et un remplacement de texte dans votre code Arduino, il suffit de suivre ces étapes :

1. Dans l'IDE Arduino, ouvrez le fichier contenant le texte que vous souhaitez remplacer.
2. Dans la barre de menu, cliquez sur "Edition" et sélectionnez "Rechercher et remplacer" (ou utilisez le raccourci clavier "Ctrl + H").
3. Dans la fenêtre de recherche et de remplacement, entrez le texte que vous souhaitez rechercher et le texte de remplacement correspondant.
4. Cliquez sur "Remplacer tout" pour effectuer les modifications dans tout le fichier.

Voici un exemple de code Arduino avec une portion de texte à remplacer :

```
void setup() {
  Serial.begin(9600);
  int ledPin = 13;
  pinMode(ledPin, OUTPUT);
}

void loop() {
  digitalWrite(LED_BUILTIN, HIGH);
  delay(1000);
  digitalWrite(LED_BUILTIN, LOW);
  delay(1000);
}
```

Supposons que nous voulons remplacer "ledPin" par "ledNum" dans tout notre code. Voici ce que nous devrions entrer dans la fenêtre de recherche et de remplacement :

Rechercher : ledPin
Remplacer par : ledNum

Résultat :

```
void setup() {
  Serial.begin(9600);
  int ledNum = 13;
  pinMode(ledNum, OUTPUT);
}

void loop() {
  digitalWrite(LED_BUILTIN, HIGH);
  delay(1000);
  digitalWrite(LED_BUILTIN, LOW);
  delay(1000);
}
```

## Plongée en profondeur

Il est important de noter que lors de la recherche et du remplacement de texte, l'ordre des caractères est strictement respecté. Cela signifie que si vous recherchez "a" et le remplacez par "b", cela ne modifiera que la première occurrence de "a" dans votre code. Si vous voulez remplacer toutes les occurrences, vous devriez utiliser l'option "Remplacer tout".

De plus, si vous ne voulez pas remplacer toutes les occurrences d'un texte, vous pouvez utiliser l'option "Remplacer" pour passer à la prochaine occurrence et décider de la remplacer ou non.

## Voir aussi

- [Documentation officielle Arduino](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [Tutoriel sur la recherche et le remplacement de texte dans l'IDE Arduino](https://www.arduino.cc/en/Tutorial/SearchAndReplaceText)
- [Tutoriel vidéo sur la recherche et le remplacement de texte dans l'IDE Arduino](https://www.youtube.com/watch?v=F9jDpK5qNtE)