---
title:                "Json 작업하기"
html_title:           "Gleam: Json 작업하기"
simple_title:         "Json 작업하기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/working-with-json.md"
---

{{< edit_this_page >}}

## 왜

JSON은 현대 프로그래밍에서 필수적인 데이터 형식입니다. Gleam에서는 JSON을 다루는 과정이 매우 간편하고 효율적입니다. JSON을 다뤄야 하는 이유가 무엇인지 자세히 알아봅시다.

## 어떻게

Gleam에서 JSON 데이터를 다루기 위한 기본적인 방법은 다음과 같습니다.

```Gleam
// JSON 데이터 생성
let person = { "name": "John", "age": 30 }

// JSON 데이터를 문자열로 변환
let json = person |> Json.from(person)

// 문자열에서 JSON 데이터로 변환
let person_again = json |> Json.to_person()
```

위 예제에서 `Json.from` 함수는 일반적인 Gleam 자료형을 JSON 문자열로 변환해줍니다. 반대로, `Json.to_person` 함수는 문자열로 된 JSON 데이터를 Gleam 자료형으로 다시 변환해줍니다.

또 Gleam에서는 `Json.Encode`와 `Json.Decode` 모듈을 이용해 더 유연하게 JSON 데이터를 다룰 수 있습니다. 아래 예제를 살펴봅시다.

```Gleam
// JSON 데이터 생성
let person = { "name": "John", "age": 30 }

// JSON 데이터를 바이트 리스트로 직렬화
let bytes = person |> Json.Encode.encode_bytes()

// 바이트 리스트를 JSON 데이터로 역직렬화
let person_again = bytes |> Json.Decode.decode_person()
```

위 예제는 데이터를 바이트 리스트로 직렬화하고 역직렬화하는 방법을 보여줍니다. 이렇게 함으로써, 다양한 데이터 형식을 다른 언어에서 사용하고 있는 경우에도 Gleam을 이용해 쉽게 JSON 데이터를 다룰 수 있습니다.

## 딥다이브

Gleam에서는 다양한 함수들을 이용해 좀 더 복잡하고 동적인 JSON 처리가 가능합니다. 예를 들어, `Json.Decode.one_of`를 사용하면 JSON 데이터에서 여러 개의 다른 필드 중 하나만 선택하여 값을 추출할 수 있습니다. 또한 `Json.Decode.field` 함수를 사용하면 JSON 데이터에서 특정 필드의 값을 읽어올 수 있습니다. 자세한 내용은 공식 Gleam 문서를 참조해주세요!

## 더 알아보기

- 공식 Gleam 문서: https://gleam.run/documentation
- JSON 라이브러리: https://github.com/arran4/gleam_json