---
title:                "현재 날짜 가져오기"
html_title:           "Ruby: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 현재 날짜
날짜를 가져오는 것은 프로그래머들에게 중요한 일입니다. 왜냐하면 현재 날짜를 알고 있다면, 무슨 일이 벌어지고 있는지를 알 수 있기 때문입니다.

## 방법:
```Ruby
puts Date.today
```
코드 블록 안에 위와 같이 입력하면, 오늘의 날짜가 출력됩니다.
```
2021-03-18
```

## 깊게 파고들기:
날짜를 가져오는 것은 오래된 컴퓨터 프로그래밍 기법 중 하나입니다. 예전에는 매우 복잡한 방법으로 날짜를 구했지만, 현재는 Ruby처럼 간편한 코드로도 날짜를 얻을 수 있습니다. 또한 날짜는 시스템 외부 API를 통해 가져올 수도 있습니다.

## 관련 자료:
- [Ruby Date 클래스 문서](https://ruby-doc.org/stdlib-3.0.0/libdoc/date/rdoc/Date.html)
- [Ruby의 시간과 날짜 처리에 대한 정리 문서](https://m.blog.naver.com/PostView.nhn?blogId=jhleefree&logNo=221221900746&proxyReferer=https:%2F%2Fwww.google.com%2F)