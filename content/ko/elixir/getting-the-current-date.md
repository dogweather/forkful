---
title:                "Elixir: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 왜?

현재 날짜를 얻는 것에 관심을 갖게 될 이유는 중요한 일들을 하기 위해서입니다. 예를 들어, 날짜를 사용하여 회원 가입 시나리오를 만들거나 만료일을 계산하는 등의 작업을 할 수 있습니다.

## 방법

일반적인 Elixir 라이브러리인 `DateTime` 모듈을 사용하여 현재 날짜를 얻을 수 있습니다. 아래 코드 블록은 `DateTime.now` 함수를 사용하여 현재 날짜와 시간을 얻는 예시입니다.

```Elixir
DateTime.now
```

위 코드를 실행하면 현재 날짜와 시간에 관한 정보가 출력됩니다.

```Elixir
# 예시 출력

{:ok, #DateTime<2021-04-05 17:05:37.412692+09:00 JST Etc/UTC>}
```

또는 날짜와 시간을 원하는 형식으로 출력할 수도 있습니다. 아래 코드 블록은 `DateTime.to_iso8601` 함수를 사용하여 현재 날짜와 시간을 ISO8601 형식으로 출력하는 예시입니다.

```Elixir
DateTime.to_iso8601(DateTime.now)
```

위 코드를 실행하면 현재 날짜와 시간이 "yyyy-mm-ddThh:mm:ssTZD" 형식으로 출력됩니다.

```Elixir
# 예시 출력

"2021-04-05T17:05:37+09:00"
```

`DateTime` 모듈 외에도 `Calendar` 모듈을 사용하여 현재 날짜와 시간을 얻을 수 있습니다. 아래 코드 블록은 `Calendar.local_time` 함수를 사용하여 현재 시간을 얻는 예시입니다.

```Elixir
Calendar.local_time
```

위 코드를 실행하면 현재 시간에 관한 정보가 출력됩니다.

```Elixir
# 예시 출력

{:ok, {{2021, 4, 5}, {17, 11, 24}}}
```

## 깊게 파고들기

`DateTime` 모듈과 `Calendar` 모듈을 사용하여 현재 날짜와 시간을 얻을 수 있지만, 더 많은 기능과 옵션을 사용할 수 있습니다. Elixir의 공식 문서를 참고하여 더 많은 정보를 확인할 수 있습니다.

## 연관 링크

- [Elixir `DateTime` 모듈 공식 문서](https://hexdocs.pm/elixir/DateTime.html)
- [Elixir `Calendar` 모듈 공식 문서](https://hexdocs.pm/elixir/Calendar.html)