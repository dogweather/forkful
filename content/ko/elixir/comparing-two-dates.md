---
title:    "Elixir: 날짜 두 개 비교하기"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## 왜

새로운 언어를 배우는 것은 항상 흥미로운 일입니다. 더 나은 프로그래밍 지식을 얻을 수 있고 새로운 도전을 수행할 수 있기 때문입니다. 이번에는 Elixir 언어에 대해 알아보고 두 날짜를 비교하는 방법을 배워보겠습니다.

## 어떻게

날짜를 비교하는 것은 매우 중요합니다. 예를 들어, 어떤 이벤트가 가장 먼저 발생했는지, 어떤 가격이 더 낮은지 및 기타 많은 상황에서 날짜 비교가 필요합니다. Elixir에서 두 날짜를 비교하는 방법을 알아보겠습니다.

먼저, 두 날짜를 비교하려면 **Calendar** 모듈의 **compare** 함수를 사용해야 합니다. 아래는 **compare** 함수를 사용하여 두 날짜를 비교하는 예제 코드입니다.

```Elixir
start_date = ~D[2020-01-01]
end_date = ~D[2020-01-05]
result = Calendar.compare(start_date, end_date)
IO.puts result
```

위의 코드에서는 먼저 **~D** 를 사용하여 날짜를 초기화하고, **Calendar.compare** 함수를 사용하여 두 날짜를 비교합니다. 이 함수는 두 번째 인자가 더 크면 1, 같으면 0, 더 작으면 -1을 반환합니다.

위의 예제 코드를 실행하면 결과로 1이 출력됩니다. 이것은 **end_date** 두 번째 인자가 **start_date** 첫 번째 인자보다 크기 때문입니다.

## 깊게 파고들기

Elixir에서 두 날짜를 비교하는 다른 방법도 있습니다. **Date** 모듈을 사용하는 것입니다. 아래는 **Date** 모듈을 사용하여 두 날짜를 비교하는 예제 코드입니다.

```Elixir
start_date = Date.new!(2020, 1, 1)
end_date = Date.new!(2020, 1, 5)
result = Date.compare(start_date, end_date)
IO.puts result
```

위의 코드에서는 **Date.new!** 함수를 사용하여 두 날짜를 초기화하고, **Date.compare** 함수를 사용하여 비교합니다. 이 함수는 두 번째 인자가 더 크면 1, 같으면 0, 더 작으면 -1을 반환합니다. 위의 예제 코드를 실행하면 결과로 1이 출력됩니다.

## 더 알아보기

Elixir에서 날짜 비교를 할 때 유용한 다른 함수들도 있습니다. **Calendar** 모듈의 **compare/3** 함수는 마지막 인자로 비교 방식을 지정할 수 있습니다. 또한, **Date** 모듈의 **Date.difference** 함수는 두 날짜 사이의 일 수를 구할 수 있습니다. 이러한 함수들을 활용하여 더 복잡한 날짜 비교를 할 수 있습니다.

## 관련 링크

- [Elixir 공식 홈페이지](https://elixir-lang.org/)
- [Elixir 문서](https://hexdocs.pm/elixir/Calendar.html)
- [Elixir Date 모듈 문서](https://hexdocs.pm/elixir/Date.html)
- [Elixir Calendar 모듈 문서](https://hexdocs.pm/elixir/Calendar.html#content)