---
title:                "Aloittaminen uusi projekti"
html_title:           "C#: Aloittaminen uusi projekti"
simple_title:         "Aloittaminen uusi projekti"
programming_language: "C#"
category:             "C#"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Uuden projektin aloittaminen tarkoittaa uuden ohjelmointityön aloittamista tyhjästä. Se voi sisältää uuden sovelluksen, kirjaston tai sivuston kehittämisen. Ohjelmoijat aloittavat uuden projektin, jotta voivat toteuttaa uusia ideoita, parantaa ja päivittää nykyisiä projektejaan tai vain oppia uusia ohjelmointikieliä ja -tekniikoita.

## Miten:

```C#
C# using System;

class MainClass {
  static void Main(string[] args) {
    Console.WriteLine("Tervetuloa uuden projektin aloittamiseen!");
    Console.WriteLine("Seuraavassa on muutamia vinkkejä, jotka auttavat sinua pääsemään alkuun:\n");

    // Luo uusi projektikansio (ohjelma) ja avaa se
    string projectName = "UusiProjekti";
    System.IO.Directory.CreateDirectory(projectName);
    System.IO.Directory.SetCurrentDirectory(projectName);

    // Luo uusi C#-tiedosto ja kirjoita siihen "Hello World!"
    using (System.IO.StreamWriter file = new System.IO.StreamWriter("Koodi.cs")) {
      file.WriteLine("using System;\n\nclass MainClass {\n\tstatic void Main(string[] args) {\n\t\tConsole.WriteLine(\"Hello World!\");\n\t}\n}");
    }

    // Käännä ja suorita koodi
    string output = "Hello World!";
    Console.WriteLine("Käännös ja suoritus:\n");
    System.Diagnostics.Process.Start("C:\\Windows\\Microsoft.NET\\Framework\\v4.0.30319\\csc.exe", "Koodi.cs").WaitForExit();
    System.Diagnostics.Process.Start("Koodi.exe").WaitForExit();
    Console.WriteLine("Ohjelman tuloste: " + output);
  }
}
```

## Syventävä sukellus:

C#-kieli on kehitetty Microsoftin toimesta vuonna 2000. Se on Microsoftin pääkieli Windows-pohjaisissa sovelluksissa, mutta sitä voi käyttää myös monilla muilla alustoilla kuten Linux ja iOS. C# on olio-orientoitunut ja sen syntaksi on hyvin samanlainen kuin Javassa.

Vaihtoehtoisia ohjelmointikieliä uuden projektin aloittamiseen ovat esimerkiksi Java, Python ja C++. Näiden kielien välillä valitessa kannattaa miettiä oman projektin tarkoitusta ja sen käyttötarkoitusta.

C#-projektin voi aloittaa myös Visual Studion avulla. Visual Studio on suosittu integroitu kehitysympäristö, joka sisältää kaiken tarvittavan ohjelmien kehittämiseen, kuten koodieditorin, debuggerin ja graafisen käyttöliittymän suunnittelijan.

## Katso myös:

- Microsoft C# -dokumentaatio https://docs.microsoft.com/en-us/dotnet/csharp/

- W3Schools C# -opetusohjelma https://www.w3schools.com/cs/

- C# -opetusvideoita YouTube-kanavalta https://www.youtube.com/playlist?list=PLAC325451207E3105