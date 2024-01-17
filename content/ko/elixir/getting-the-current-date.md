---
title:                "현재 날짜 받아오기"
html_title:           "Elixir: 현재 날짜 받아오기"
simple_title:         "현재 날짜 받아오기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 지금 뭐 & 왜?

현재 날짜 받기는 프로그래머들이 현재 시각을 얻기 위해 사용하는 것이다.

## 어떻게:

```Elixir
Date.utc_today()
```

위의 코드는 Elixir에서 현재 날짜를 얻는 방법의 가장 간단한 예시이다. 실제로 실행하면 아래와 같은 결과가 나온다.

```Elixir
~D[2019-08-20]
```

위 코드에서 `~D`는 날짜를 나타내는 데이터 타입을 의미하고, 대괄호 안에 실제 날짜 정보가 나타나게 된다.

## 좀 더 파헤쳐보기:

Elixir에서 현재 날짜를 얻는 방법은 `Date.utc_today()` 말고도 몇 가지가 있다. `Date.local_today()`는 현재 로컬 시각을 기준으로 날짜를 얻는 방법이다. 또한 `DateTime.utc_now()`와 `DateTime.local_now()`는 시간까지 포함하여 현재 시각을 얻는 방법이다.

만약 Elixir가 아닌 다른 프로그래밍 언어를 사용하고 있다면, 현재 날짜를 얻는 다른 방법도 있을 수 있다. `Date()` 함수를 사용하는 방법이 대표적인 예시이다. 하지만 이 방법은 시간 정보를 제공하지 않으므로, 정확한 시각이 필요한 경우에는 `DateTime()` 함수를 사용하는 것이 더 적절하다.

## 더 알아보기:

- [Elixir Date 모듈 문서](https://hexdocs.pm/elixir/Date.html)
- [Elixir DateTime 모듈 문서](https://hexdocs.pm/elixir/DateTime.html)
- [Datetime vs Date in Elixir](https://stackoverflow.com/questions/45527853/datetime-vs-date-in-elixir)

## 참고하기:

- `Date.now()` 와 같은 다른 프로그래밍 언어에서 현재 날짜를 얻는 방법이 있다면, 참고하여 사용해보는 것도 좋은 방법이다. 다른 방법을 알면 더 효율적으로 코드를 작성할 수 있을 것이다.