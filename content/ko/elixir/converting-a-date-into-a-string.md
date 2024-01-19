---
title:                "날짜를 문자열로 변환하기"
html_title:           "Arduino: 날짜를 문자열로 변환하기"
simple_title:         "날짜를 문자열로 변환하기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

날짜를 문자열로 변환하는 것은 특정 날짜를 처리 가능한 텍스트 형식으로 바꾸는 작업입니다. 이 작업은 데이터를 저장하거나 표현할 때 직관적 이해를 돕고, 서로 다른 시스템이 데이터를 공유하도록 만들어 주기 때문에 프로그래머들이 필요로 합니다.

## 어떻게:

```Elixir
date = ~D[2022-01-01]
IO.inspect(Date.to_iso8601(date))
```
위 코드는 '2022-01-01'이라는 문자열을 출력합니다.

## Deep Dive:

날짜를 문자열로 변환하는 작업은 프로그래밍의 초기 시절부터 존재했습니다. 이 방법이 있었기에 다양한 시스템 간에 데이터를 공유하는 것이 가능했습니다. 현재는 ISO 8601, POSIX, RFC 3339 등 다양한 표준들이 제공되며, Elixir에서는 `Date.to_iso8601/1` 메서드를 제공하여 ISO 8601 형식의 문자열을 쉽게 얻을 수 있습니다.

## 참고자료:

- Elixir 공식 사이트: [https://elixir-lang.org](https://elixir-lang.org)
- ISO 8601: [http://www.iso.org/iso/home/standards/iso8601.htm](http://www.iso.org/iso/home/standards/iso8601.htm)
- Erlang's calendar module (on which Elixir's Date module is based): [http://erlang.org/doc/man/calendar.html](http://erlang.org/doc/man/calendar.html)
- POSIX: [https://en.wikipedia.org/wiki/POSIX](https://en.wikipedia.org/wiki/POSIX)
- RFC 3339: [https://www.ietf.org/rfc/rfc3339.txt](https://www.ietf.org/rfc/rfc3339.txt)