---
title:                "현재 날짜 가져오기"
html_title:           "C: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 무엇인가 & 왜 필요한가? 
현재 날짜를 가져오는 것은 프로그램이 현재 어떤 날짜인지 알 수 있게 하는 기능입니다. 이는 로그 기록, 데이터 타임스탬픙, 또는 날짜에 따른 기능 제어 등 다양한 목적으로 프로그래머들에게 사용됩니다.

## 어떻게 하는가:
간단한 코딩 예제와 출력입니다:

```Ruby
require 'date'

# 현재 날짜를 가져옵니다.
today = Date.today
puts today
```
 
이 명령어를 실행하면 아래와 같은 결과를 볼 수 있습니다:
  
```Ruby
2022-04-01
```
## 심층 분석:
**역사적 배경:** Ruby에서 'Date' 클래스는 날짜 관련 작업을 단순화하기 위해 1990년대 후반에 도입되었습니다. 이전에는 이러한 작업을 수행하는 것은 상당히 복잡했습니다.

**대안:** 'Time' 클래스를 사용하여 현재 날짜를 얻을 수도 있습니다. 다음과 같이 사용할 수 있습니다:

```Ruby
require 'time'

# 현재 날짜를 가져옵니다.
currentTime = Time.new
puts currentTime.strftime("%Y-%m-%d")
```
**구현 세부 정보:** 'Date.today'는 내부적으로 'Time.new'를 호출하여 현재 시스템 시간을 얻습니다. 그런 다음 그 시간 정보를 'Date' 객체로 변환하여 날짜 정보를 제공합니다.

## 참고하기:
- Ruby 공식 문서: Date 클래스 (https://ruby-doc.org/stdlib-3.0.1/libdoc/date/rdoc/Date.html)
- Ruby 공식 문서: Time 클래스 (https://ruby-doc.org/stdlib-3.0.1/libdoc/time/rdoc/Time.html)