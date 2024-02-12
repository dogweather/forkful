---
title:                "연관 배열 사용하기"
aliases:
- ko/elixir/using-associative-arrays.md
date:                  2024-01-30T19:10:51.302370-07:00
model:                 gpt-4-0125-preview
simple_title:         "연관 배열 사용하기"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?

Elixir에서 연관 배열은 Maps라고 불리며, 고유한 키가 값을 가리키는 키-값 쌍의 모음입니다. Maps는 데이터를 즉석에서 저장하고 검색하는 데 매우 편리하여 코드를 깨끗하게 정리하고 일상생활을 더욱 쉽게 만들어 줍니다.

## 사용 방법:

Map을 생성하는 것은 간단합니다. `%{}` 문법을 사용하면 됩니다.

```elixir
my_map = %{"name" => "Alex", "age" => 32}
IO.inspect(my_map)
```

값에 접근하는 것은 키를 사용하여 수행됩니다.

```elixir
IO.puts my_map["name"]
```
출력: `Alex`

값을 추가하거나 업데이트하려면 `Map.put/3` 함수를 사용할 수 있습니다.

```elixir
updated_map = Map.put(my_map, "location", "NY")
IO.inspect(updated_map)
```
출력: `%{"age" => 32, "location" => "NY", "name" => "Alex"}`

키를 제거하는 것은 `Map.delete/2`를 사용하여 마찬가지로 간단합니다.

```elixir
trimmed_map = Map.delete(updated_map, "age")
IO.inspect(trimmed_map)
```
출력: `%{"location" => "NY", "name" => "Alex"}`

## 심화 학습

Elixir의 Maps은 루비의 해시나 파이썬의 딕셔너리와 같은 이전의 키-값 저장 타입의 진화입니다. Maps은 더 효율적인 조회와 삽입을 허용하며, 현대 Elixir 프로그래밍에 있어 가장 선호되는 방법입니다. Maps 이전에 Elixir가 사용한 HashDict와 Dict 모듈은 이제 사용되지 않는다는 점을 알아두는 것이 중요합니다.

그러나 순서가 있는 데이터가 필요한 시나리오의 경우, Elixir의 키워드 리스트를 살펴볼 수 있습니다. 이는 튜플의 리스트로써, 작은 컬렉션에는 효율적이지만 대규모 데이터셋에는 Maps만큼 성능이 좋지 않습니다.

Maps은 키를 "평평한" 구조로 저장하므로, 중첩된 값을 직접 접근하는 것이 조금 까다롭습니다. 깊은 중첩에 대해서는 `get_in`, `put_in`, `update_in`, `get_and_update_in` 함수를 통해 보다 동적인 접근을 허용하며, 중첩된 데이터 조작에 대한 구조화된 접근을 고려할 수 있습니다.

요약하자면, Maps은 Elixir에서 연관 배열 필요성에 대한 해답이지만, 언어는 모든 시나리오에 대해 다양한 데이터 구조를 제공하며, 작업에 적합한 도구를 선택하도록 장려합니다.
