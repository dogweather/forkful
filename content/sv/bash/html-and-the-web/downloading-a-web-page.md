---
date: 2024-01-20 17:43:46.150282-07:00
description: "S\xE5 h\xE4r g\xF6r du: Enkel nedladdning med `curl`."
lastmod: '2024-03-13T22:44:38.081375-06:00'
model: gpt-4-1106-preview
summary: Enkel nedladdning med `curl`.
title: "H\xE4mta en webbsida"
weight: 42
---

## Så här gör du:
Enkel nedladdning med `curl`:
```Bash
curl https://www.example.com -o example_page.html
```
Så här ser resultatet ut:
```Bash
% Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                              Dload  Upload   Total   Spent    Left  Speed
100  1270  100  1270    0     0   6350      0 --:--:-- --:--:-- --:--:--  6350
```
För att tysta output använd `-s` flaggan:
```Bash
curl -s https://www.example.com -o example_page.html
```
För att använda `wget`:
```Bash
wget https://www.example.com
```
Filen sparas som `index.html` av default.

## Djupdykning
Historiskt sett har `wget` varit standardverktyget för att ladda ned och spegla hela webbsidor eller webbplatser, medan `curl` växte fram som det flexiblare verktyget för dataöverföringar av alla slag. `wget` är rekursiv, vilket betyder att den kan ladda ned hela webbplatser genom att följa länkar, medan `curl` hanterar enstaka filer effektivt.

Alternativ till dessa inkluderar mer specialiserade verktyg som `HTTrack` eller att skriva anpassade skript med hjälp av `Python` och bibliotek som `Requests` och `BeautifulSoup` för webbskrapning.

Implementationen av filnedladdning i Bash kan variera beroende på operativsystemet. MacOS till exempel levereras med `curl`, men inte `wget` som default. Såväl `curl` som `wget` hanterar HTTP-protokollet, men `curl` stöder även ett stor antal andra protokoll som FTP, SMTP och LDAP.

## Se även
- Curl projektets officiella webbplats: [https://curl.se/](https://curl.se/)
- Wget manualen: [https://www.gnu.org/software/wget/manual/wget.html](https://www.gnu.org/software/wget/manual/wget.html)
- Web scraping med Python: [https://docs.python-requests.org/en/latest/](https://docs.python-requests.org/en/latest/) samt [https://www.crummy.com/software/BeautifulSoup/](https://www.crummy.com/software/BeautifulSoup/)
