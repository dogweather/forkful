---
title:                "날짜를 문자열로 변환하기"
html_title:           "Ruby: 날짜를 문자열로 변환하기"
simple_title:         "날짜를 문자열로 변환하기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 뭐고 왜?

날짜를 문자열로 변환한다는 것은 무엇인가요? 프로그래머들이 왜 이렇게 하는 걸까요? 문자열로 날짜를 변환하면, 날짜 입력을 플랫폼마다 인식할 수 있는 형식으로 변환할 수 있어서 매우 유용합니다. 예를 들어, 02/03/2021 같은 날짜를 2021년 2월 3일로 해석하고 싶다면, 문자열로 변환해야 합니다.

## 어떻게:

```Ruby
date = Date.new(2021, 2, 3)
puts date.to_s
```
```
2021-02-03
```

위 코드를 실행하면, 날짜 프로퍼티를 지정하여 날짜 객체를 만들고, ```to_s``` 메소드를 사용하여 문자열로 변환할 수 있습니다. ```to_s```는 간단한 예시로, 실제 프로그래밍에서는 날짜 포맷을 더 다양하고 복잡하게 지정할 수 있습니다.

## 깊이 파고들기:

지금까지 날짜 객체를 문자열로 변환하는 방법만 알아보았지만, 날짜를 표현하는 다양한 형식이 있으므로 참고하면 좋습니다. 예를 들어, 날짜가 다른 형식으로 입력되었을 때도 인식할 수 있도록 처리하는 기능이 필요할 수 있습니다. 이 때는 날짜 포맷을 지정하는 코드를 작성하여 처리할 수 있습니다.

또한, 날짜 객체를 문자열로 변환하는 또 다른 방법으로는 ```strftime``` 메소드를 사용하는 것이 있습니다. 이 메소드는 ```to_s```와 비슷하지만, 포맷을 더 다양하게 지정할 수 있습니다. 이를 활용하면 날짜 포맷을 더 자유롭게 지정할 수 있게 됩니다.

마지막으로, 날짜 객체를 문자열로 변환하는 방법은 다양하지만, 어떤 방법을 선택하느냐에 따라 다양한 결과가 나올 수 있으므로, 역시 프로그래밍에 따라서 구체적인 방법을 선택해야 합니다.

## 더 알아보기:

- [Ruby 날짜 포맷 가이드](https://www.tutorialspoint.com/ruby/ruby_date_time.htm)
- [날짜 포맷에 대한 더 자세한 설명](https://www.rubydoc.info/docs/ruby-doc-bundle/Date:strftime)
- [날짜와 시간 관련 기능들 참고하기](https://ruby-doc.org/stdlib-3.0.0/libdoc/date/rdoc/DateTime.html)