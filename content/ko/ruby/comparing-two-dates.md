---
title:                "두 날짜 비교하기"
html_title:           "C#: 두 날짜 비교하기"
simple_title:         "두 날짜 비교하기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

# 날짜 비교란 무엇인가, 왜 하는가?

두 날짜 비교는 하나의 날짜가 다른 날짜보다 빠른지, 늦은지 아니면 같은 시간인지를 판단하는 것입니다. 이것은 스케줄링, 이벤트 정렬, 시간 경과 측정 등 다양한 프로그래밍 작업에서 필수적입니다.

# 이렇게 하십시오:

Ruby에서 두 날짜를 비교하기 위해 '<%=와 '%=>' 연산자를 사용하는 방법을 보여줍니다.

```Ruby
require 'date'

date1 = Date.new(2022, 1, 1)
date2 = Date.new(2023, 1, 1)

if date1 < date2
  puts "date1 is earlier than date2"
elsif date1 > date2
  puts "date1 is later than date2"
else
  puts "Both dates are identical"
end
```

출력:

```Ruby
date1 is earlier than date2
```

# 자세히 알아보기

**역사적 맥락** : Ruby의 Date 클래스는 1997년에 등장한 Ruby 1.0부터 있었습니다. 직관적인 인터페이스로 빠르게 인기를 얻었습니다.

**대안들** : Ruby 외의 다른 언어들, 예를 들어 Python과 Java도 날짜 비교 기능을 제공합니다. 서로 다른 언어들이 제공하는 날짜 비교 방식은 비슷하지만, 각각의 언어에 맞는 독특한 사용법이 있습니다.

**구현 세부사항** : Ruby의 Date 클래스는 비교를 위해 연산자를 오버로딩합니다. 즉, 두 날짜 간의 비교는 실제로 연산자를 사용한 것으로 취급되어, 해당 연산자가 정의하는 방식으로 비교됩니다.

# 참고 자료

관련 주제에 대한 더 자세한 설명을 찾으신다면 아래의 링크를 참고하십시오:

Ruby Date 공식 문서: https://ruby-doc.org/core-Date.html
"Comparing Dates" 설명서: https://www.tutorialspoint.com/ruby/ruby_date_time.htm
Python 날짜 비교: https://docs.python.org/3/library/datetime.html
Java 날짜 비교: https://docs.oracle.com/javase/7/docs/api/java/util/Date.html