---
title:                "Json 작업하기"
html_title:           "Elixir: Json 작업하기"
simple_title:         "Json 작업하기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/working-with-json.md"
---

{{< edit_this_page >}}

#  뭐고 왜?
JSON 작업이란 무엇인가? 프로그래머들이 왜 그런 작업을 할까?

JSON(JavaScript Object Notation) 작업은 주로 데이터를 저장하고 전송하기 위해 사용되는 특정 형식의 데이터 표현 방식입니다. 이러한 작업은 웹 애플리케이션에서는 물론 모바일과 같은 여러 플랫폼에서도 일반적으로 사용됩니다. 이 형식은 간결하고 읽기 쉽기 때문에 매우 인기가 있습니다.

# 방법
이제 우리가 그것의 중요성을 이해했다면, Elixir에서 JSON 작업을 어떻게 할 수 있는지 알아보겠습니다. 아래의 코드 블록을 참조하세요.

```Elixir
# JSON 모듈 가져오기
import Jason

# 예시 데이터
data = %{name: "John", age: 30, hobbies: ["programming", "hiking"]}

# 데이터를 JSON 형식으로 변환
json_data = Jason.encode!(data)

# JSON을 다시 Elixir 데이터로 변환
elixir_data = Jason.decode!(json_data)

# 변환된 데이터 확인
IO.inspect(elixir_data)

# 출력 결과
%{age: 30, hobbies: ["programming", "hiking"], name: "John"}
```

# 깊게 들어가기
JSON은 현재 매우 널리 사용되는 형식입니다. 그러나 실제로 이전에는 XML이 주로 사용되었으며, 많은 개발자들은 JSON을 더 선호합니다. JSON이 간결하고 읽기 쉽고 데이터 전송 및 저장에 유용하기 때문입니다.

혹은 Elixir의 내장 모듈인 Poison을 사용하여 JSON 작업을 수행할 수도 있습니다.

JSON과 같은 구조화된 데이터 작업을 하고자 한다면, Elixir에서는 key-value pair를 사용하는 이 일반적인 방법을 선호합니다. 데이터 업데이트는 매우 빠르게 처리할 수 있으며 필요한 경우 해시 테이블을 사용하여 데이터를 더욱 효율적으로 관리할 수 있습니다.

# 더 알아보기
더 많은 정보를 원하신다면 다음 링크를 확인해보세요:

- https://elixir-lang.org/docs.html : Elixir 공식 문서
- https://www.json.org/ : JSON 정보 및 규격
- https://github.com/michalmuskala/poison : Elixir의 내장 모듈인 Poison에 대한 정보