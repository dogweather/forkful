---
title:                "현재 날짜 가져오기"
html_title:           "C: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 사용하는가?
현재 날짜를 가져오는 것이란 현재의 시간 정보를 볼 때 사용하는 프로그래밍 함수입니다. 이는 타임스탬프 생성 또는 사용자에게 날짜 정보를 제공하는 등 다양한 상황에서 사용됩니다.

## 어떻게 사용하는가:
Gleam 언어에서 현재 날짜를 가져오는 방법은 다음과 같습니다.

```Gleam
import gleam/date.{now, format}

fn main() {
  let current_date = now()
  format(current_date) // "2022-05-22"
}
```

이 코드를 실행하면 2022년 5월 22일과 같은 현재 날짜를 받을 수 있습니다.

## 깊이 알아보기
현재 날짜를 가져오는 것은 거의 모든 프로그래밍 언어에서 사용하는 기본적인 기능입니다. 이 기능은 파일이나 데이터베이스 항목의 타임스탬프를 생성하거나 특정 일자의 이벤트를 로깅하는데 사용됩니다.

대안으로는 시스템 시간을 직접 조회하거나 특정 서비스를 이용하는 방법이 있습니다. 하지만 이 경우 시간대 문제나 네트워크 지연 등 추가적인 문제를 고려해야 합니다.

Gleam 프로그래밍 언어에서는 `gleam/date` 라이브러리를 통해 이 기능을 단순하고 정확하게 사용할 수 있습니다. 이 라이브러리는 더 많은 날짜와 시간 관련 기능을 제공하며, 또한 시간대를 고려한 날짜와 시간 계산도 가능합니다.

## 참고 자료
날짜와 시간에 대한 Gleam 프로그래밍의 더 자세한 내용은 아래 링크를 참조하세요:

2. [Gleam 시간대 처리에 대한 블로그 글](https://gleam.run/news/gleam-v0.14-released/)
3. [ISO 8601 날짜와 시간 표준에 대한 위키백과 글](https://en.wikipedia.org/wiki/ISO_8601)