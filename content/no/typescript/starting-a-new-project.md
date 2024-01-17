---
title:                "Å starte et nytt prosjekt"
html_title:           "TypeScript: Å starte et nytt prosjekt"
simple_title:         "Å starte et nytt prosjekt"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Hva og hvorfor?
Når vi sier at vi skal starte et nytt prosjekt, betyr det at vi skal begynne å kode et nytt program eller applikasjon. Dette kan være for å løse et problem, lage noe nytt eller forbedre noe eksisterende. Programmerere starter nye prosjekter for å utvide sine ferdigheter, utforske nye konsepter og skape noe unikt.

## Slik gjør du det:
```TypeScript
const newProject = (name: string, languages: string[]) => {
  console.log(`Starting new project: ${name}`);
  console.log(`Using ${languages.join(", ")} languages`);
}

newProject("Awesome App", ["TypeScript", "HTML", "CSS"]);

// Output:
// Starting new project: Awesome App
// Using TypeScript, HTML, CSS languages
```

## Dypdykk:
Å starte et nytt prosjekt har blitt mye enklere med utviklingen av programmeringsspråket TypeScript. Dette språket bruker syntaks som er lik JavaScript, men legger til typetilordning og kompilering for å unngå feil og gjøre koden mer strukturert. Alternativene til TypeScript inkluderer JavaScript og andre programmeringsspråk, men TypeScript er populært på grunn av sin enkle syntaks og fordelene den gir til store og komplekse prosjekter. For å starte et prosjekt med TypeScript, må du installere TypeScript kompilatoren og konfigurere utviklingsmiljøet ditt til å støtte det.

## Se også:
- Offisiell TypeScript dokumentasjon: https://www.typescriptlang.org/docs/
- En detaljert guide for å komme i gang med TypeScript: https://www.tutorialspoint.com/typescript/typescript_quick_guide.htm 
- TypeScript eksempler og prosjekter: https://github.com/topics/typescript