---
date: 2024-01-20 17:44:05.141254-07:00
description: "How to: (Jak to zrobi\u0107:) Je\u015Bli wszystko przebiegnie pomy\u015B\
  lnie, zobaczysz utworzony plik `example_page.html` z zawarto\u015Bci\u0105 strony\
  \ `example.com`."
lastmod: '2024-04-05T21:53:37.267174-06:00'
model: gpt-4-1106-preview
summary: "(Jak to zrobi\u0107:) Je\u015Bli wszystko przebiegnie pomy\u015Blnie, zobaczysz\
  \ utworzony plik `example_page.html` z zawarto\u015Bci\u0105 strony `example.com`."
title: Pobieranie strony internetowej
weight: 42
---

## How to: (Jak to zrobić:)
```Fish Shell
function fetch_page -d "Download web page content"
    set url $argv[1]
    set output $argv[2]
    curl $url -o $output
end

fetch_page "http://example.com" "example_page.html"
```

Jeśli wszystko przebiegnie pomyślnie, zobaczysz utworzony plik `example_page.html` z zawartością strony `example.com`.

## Deep Dive (Dogłębna analiza)
Zanim `curl` stał się standardem, używano `wget` do ściągania plików. Dzisiaj `curl` jest preferowany ze względu na elastyczność i wsparcie dla wielu protokołów. Fish Shell to nowoczesna powłoka, która ułatwia życie programisty, m.in. przez wbudowane funkcje, takie jak wyżej definiowane `fetch_page`. Umożliwiają one tworzenie skryptów, które integrują różne narzędzia w bardziej czytelne i zarządzalne bloki kodu.

Gdy ściągasz stronę, pamiętaj o prawach autorskich i zasadach korzystania ze strony — nie wszystkie treści są dozwolone do pobrania. Dodatkowo, zwróć uwagę na `robots.txt` danego serwisu, gdzie określone mogą być zasady skanowania strony przez boty.

## See Also (Zobacz również)
- [Official Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- [cURL Project](https://curl.se/)
- [Wget Manual](https://www.gnu.org/software/wget/manual/wget.html)
- [HTTP client/server technology for Fish](https://github.com/fish-shell/fish-shell/wiki/HTTP-client/server-technology)
