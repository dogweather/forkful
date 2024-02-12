---
title:                "Skriving til standardfeil"
aliases:
- no/ruby/writing-to-standard-error.md
date:                  2024-02-03T19:34:18.014625-07:00
model:                 gpt-4-0125-preview
simple_title:         "Skriving til standardfeil"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å skrive til standardfeil (stderr) i Ruby handler om å dirigere feilmeldinger eller diagnostikk til en separat utstrøm, forskjellig fra standardutgangen (stdout). Programmerere gjør dette for å skille vanlig programutskrift fra feil og feilsøkingsinformasjon, noe som letter diagnosen av problemer og parsing av logger.

## Hvordan:
Rubys standardbibliotek gir en enkel måte å skrive til stderr ved å bruke `$stderr` eller `STDERR`. Du trenger ikke tredjepartsbiblioteker for denne grunnleggende operasjonen.

### Skrive en enkel melding til stderr:
```ruby
$stderr.puts "Feil: Fil ikke funnet."
# Eller tilsvarende
STDERR.puts "Feil: Fil ikke funnet."
```
Eksempelutskrift (til stderr):
```
Feil: Fil ikke funnet.
```

### Omdirigere stderr til en fil:
```ruby
File.open('error.log', 'w') do |file|
  STDERR.reopen(file)
  STDERR.puts "Mislyktes i å åpne konfigurasjonen."
end
```
Denne kodesnutten omdirigerer stderr til en fil med navnet `error.log`, og alle påfølgende skrevne feil vil bli utskrevet der til programmet tilbakestiller omdirigeringen av stderr eller avslutter.

### Bruke stderr med unntakshåndtering:
```ruby
begin
  # Simulerer en operasjon som kunne feile, f.eks., å åpne en fil
  File.open('nonexistent_file.txt')
rescue Exception => e
  STDERR.puts "Unntak oppsto: #{e.message}"
end
```
Eksempelutskrift (til stderr):
```
Unntak oppsto: No such file or directory @ rb_sysopen - nonexistent_file.txt
```

Selv om Rubys innebygde metoder for å skrive til stderr er tilstrekkelige for mange applikasjoner, kan du vurdere standardbiblioteket `logger` eller eksterne gems som `Log4r` for mer komplekse loggingsbehov. Disse tilbyr konfigurerbare loggingsmekanismer, inkludert alvorlighetsnivåer, formatering og muligheten til å skrive til ulike utdata, inkludert filer, e-post og mer.
