---
title:                "Lese en tekstfil"
aliases:
- no/ruby/reading-a-text-file.md
date:                  2024-01-20T17:55:14.604834-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lese en tekstfil"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Lese en tekstfil i Ruby betyr å hente data fra en fil som er lagret på disken. Programmerere gjør dette for å behandle informasjon som er bevart over tid, utenfor et program.

## How to:
```Ruby
# Enkel lesing av en hel fil
contents = File.read('eksempel.txt')
puts contents

# Linje for linje lesing
File.foreach('eksempel.txt') { |line| puts line }

# Lesing med en filhåndterer
File.open('eksempel.txt', 'r') do |file|
  file.each_line do |line|
    puts line
  end
end

# Håndtering av unntak hvis filen ikke finnes
begin
  contents = File.read('ikkeeksisterende.txt')
rescue Errno::ENOENT => e
  puts "Filen ble ikke funnet: #{e.message}"
end
```
```Output
Dette er innholdet i eksempel.txt filen.

Dette er innholdet i eksempel.txt filen, ønsket visning av linje for linje.

Dette er innholdet i eksempel.txt filen, når leses gjennom en filhåndterer.

Filen ble ikke funnet: No such file or directory @ rb_sysopen - ikkeeksisterende.txt
```

## Deep Dive:
Lesing av tekstfiler i Ruby har røtter tilbake til UNIX-filosofien, der alt er en fil. Dette enkle konseptet gjør det kraftfullt og fleksibelt å jobbe med data i mange former.

Alternativer til `File.read` inkluderer biblioteker som `CSV` for kommadelte verdier eller `YAML` og `JSON` parsers for strukturerte data. En `IO` klasse er også tilgjengelig for lavnivå I/O-operasjoner.

Implementasjonsdetaljer blir viktige når man håndterer store filer. For eksempel, `File.read` leser hele filen til minnet som kan være problematisk med veldig store filer. Å bruke `File.foreach` eller `File.open` med en blokk lar oss behandle hver linje separat, noe som er mer minneeffektivt.

## See Also:
- Ruby's I/O-klassedokumentasjon: [Ruby IO Docs](https://ruby-doc.org/core-2.7.0/IO.html)
- Lær om `CSV` biblioteket: [Ruby CSV Docs](https://ruby-doc.org/stdlib-2.6/libdoc/csv/rdoc/CSV.html)
- Ytterligere lesing på filbehandling i Ruby: [Ruby File Docs](https://ruby-doc.org/core-2.7.0/File.html)
