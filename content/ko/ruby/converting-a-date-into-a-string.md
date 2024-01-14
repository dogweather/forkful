---
title:                "Ruby: 날짜를 문자열로 변환하기"
simple_title:         "날짜를 문자열로 변환하기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 왜

날짜를 문자열로 변환하는 것이 왜 중요한지 궁금하신가요? 우리는 소프트웨어를 만들때 종종 사용자에게 날짜를 보여주어야 합니다. 이때 문자열로 변환하여 적절하게 표현하는 것이 필요합니다.

## 어떻게

```Ruby
date = Time.now # 현재 시간을 가져옵니다.
date.to_s       # date를 문자열로 변환합니다.
```

```Ruby
date = Time.new(2021, 12, 25) # 2021년 12월 25일을 나타내는 date 객체를 생성합니다.
date.strftime("%B %d, %Y")    # "December 25, 2021"과 같이 원하는 형식으로 문자열로 변환합니다.
```

출력 예시:

"2021-08-25 16:55:27 +0900"

"December 25, 2021"

## 더 깊이 들어가기

일반적으로 Ruby에서는 Time 라이브러리를 사용하여 날짜와 시간을 다룹니다. Time 객체를 문자열로 변환하는 방법에는 `to_s` 메소드와 `strftime` 메소드가 있습니다. `to_s` 메소드는 기본적인 날짜와 시간을 문자열로 변환하기에 적합하지만 원하는 형식으로 표현하기 위해서는 `strftime` 메소드를 사용하는 것이 더 좋습니다. 이 메소드는 strftime 지정자를 사용하여 원하는 형식으로 날짜와 시간을 표현할 수 있습니다.

## See Also

- [Ruby Time 클래스](https://ruby-doc.org/core-3.0.1/Time.html)
- [Ruby strftime 지정자](https://ruby-doc.org/core-3.0.1/Time.html#method-i-strftime)