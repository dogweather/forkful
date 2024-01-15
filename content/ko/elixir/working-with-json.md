---
title:                "json과 함께하는 작업"
html_title:           "Elixir: json과 함께하는 작업"
simple_title:         "json과 함께하는 작업"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/working-with-json.md"
---

{{< edit_this_page >}}

## 왜

JSON은 현대의 프로그래밍에서 거의 필수적인 데이터 형식입니다. Elixir에서 JSON을 다루는 법을 배우면 데이터를 더 효율적이고 쉽게 다룰 수 있습니다.

## 어떻게

```Elixir
# JSON 라이브러리 임포트
alias Jason, as: JSON
# Elixir 맵을 JSON 문자열로 변환
json = JSON.encode(%{name: "John", age: 26})
# 출력: "{\"name\": \"John\", \"age\": 26}"

# JSON 문자열을 Elixir 맵으로 변환
map = JSON.decode("{\"name\": \"John\", \"age\": 26}")
# 출력: %{name: "John", age: 26}
```

## 딥 다이브

JSON은 텍스트 형식의 데이터를 저장하고 교환하기 위해 개발된 경량의 포맷입니다. Elixir에서는 JSON 라이브러리를 사용하여 데이터를 쉽게 맵으로 변환하고, 맵을 다시 JSON 문자열로 변환할 수 있습니다. 또한 Elixir의 많은 라이브러리들이 JSON을 지원하기 때문에 다른 라이브러리와 연동하기 쉽습니다.

## See Also

- [Elixir 공식 문서][1]
- [JSON.org][2]
- [ElixirSchool에서 Elixir로 JSON 다루는 법 배우기][3]

[1]: https://hexdocs.pm/elixir/JSON.html
[2]: https://www.json.org/
[3]: https://elixirschool.com/ko/lessons/specifics/json/