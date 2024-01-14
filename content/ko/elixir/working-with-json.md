---
title:                "Elixir: JSON 작업하기"
simple_title:         "JSON 작업하기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/working-with-json.md"
---

{{< edit_this_page >}}

## 왜

Elixir 프로그래밍은 대규모 앱을 만들고 관리하는데 아주 효율적인 언어입니다. 이 언어에는 다양한 라이브러리가 있어서 데이터 형식을 다루는 데에도 용이합니다. 그 중에서도 JSON은 인터넷에서 자주 쓰이는 데이터 형식 중 하나입니다. 따라서 Elixir를 사용하면 쉽게 JSON 데이터를 다룰 수 있습니다.

## 사용 방법

JSON 데이터를 Elixir에서 다루기 위해선, 우선적으로 `Poison` 라이브러리를 설치해야 합니다. 그 후 다음과 같이 코드를 작성할 수 있습니다:
```
Elixir

json_data = ~s({"name":"Kim","age":25})
parsed_json = Poison.decode(json_data)
IO.puts(parsed_json["name"])
```

코드를 실행하면 다음과 같은 결과가 나옵니다:
```
Kim
```

위 코드에서 `~s`는 Elixir에서 문자열을 표현하는 방식 중 하나입니다. 만약 파일에서 JSON 데이터를 읽고 싶다면, 다음과 같이 할 수 있습니다:
```
Elixir

json_file = File.read!("data.json")
parsed_json = Poison.decode(json_file)
IO.puts(parsed_json["age"])
```

`data.json` 파일의 내용이 `{"name":"Kim","age":25}`라면 위코드는 `25`를 출력합니다.

## 깊게 살펴보기

`Poison`은 Elixir에서 가장 보편적으로 사용되는 JSON 라이브러리입니다. 하지만 만약 좀 더 유연하고 다양한 기능을 가진 라이브러리를 찾고 있다면 `Jason`을 사용해보는 것도 좋은 방법입니다. `Jason`은 더 나은 성능과 유연한 문법을 제공합니다. 또한 `Ecto` 라이브러리와 함께 사용하면 데이터베이스와의 작업에서도 매우 유용합니다.

## 또 다른 정보

- Elixir 공식 홈페이지: [https://elixir-lang.org/](https://elixir-lang.org/)
- Poison 라이브러리 문서: [https://hexdocs.pm/poison/readme.html](https://hexdocs.pm/poison/readme.html)
- Jason 라이브러리 문서: [https://hexdocs.pm/jason/readme.html](https://hexdocs.pm/jason/readme.html)
- Ecto 라이브러리 문서: [https://hexdocs.pm/ecto/Ecto.html](https://hexdocs.pm/ecto/Ecto.html)

## 더 보기