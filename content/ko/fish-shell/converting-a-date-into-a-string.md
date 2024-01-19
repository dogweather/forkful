---
title:                "날짜를 문자열로 변환하기"
html_title:           "Arduino: 날짜를 문자열로 변환하기"
simple_title:         "날짜를 문자열로 변환하기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 무엇이며 왜?

날짜를 문자열로 변환하는 것은, 날짜 데이터를 텍스트 형식으로 바꾸는 프로세스입니다. 이는 프로그래머가 사용자 친화적인 형식으로 출력하거나, 텍스트와 함께 날짜 데이터를 조작하고 저장하기 위해 수행됩니다.

## 어떻게 하는가:

Fish Shell에서 날짜를 문자열로 변환하는 방법은 다음과 같습니다:

```Fish Shell
set dateString (date "+%Y-%m-%d")
echo $dateString
```
위 코드를 실행하면 오늘 날짜를 'YYYY-MM-DD' 형식의 문자열로 출력합니다.

## 깊게 알아보기:

날짜를 문자열로 변환하는 기능은 프로그래밍의 핵심 부분입니다. 'Date' 개체를 만들고 텍스트 형식으로 변환하는 작업은 웹 애플리케이션, 데이터베이스 사용 등 많은 경우에 필요합니다. 

Fish Shell에서는 POSIX에서 정의한 'date' 명령어를 사용해 날짜를 문자열로 변환합니다. 이 방식은 Fish Shell 이전의 쉘 프로그래밍에서도 주로 사용되었습니다. 그러나 다른 언어나 프레임워크에서는 별도의 날짜 변환 함수를 제공하기도 합니다. 예를 들어, Python에서는 'strftime' 함수를, JavaScript에서는 'Date.prototype.toISOString' 함수를 사용할 수 있습니다.

Fish Shell의 'date' 명령어는 시스템의 지역설정을 기반으로 합니다. 따라서 서로 다른 시스템에서 실행할 경우, 출력 될 문자열이 서로 다를 수 있습니다.

## 참고하면 좋을 링크들: 
* [Fish Shell 공식 문서](https://fishshell.com/docs/current/index.html)
* [POSIX의 date 명령어에 대한 설명](http://pubs.opengroup.org/onlinepubs/9699919799/utilities/date.html)
* [날짜를 문자열로 변환하는 다른 방법에 대한 Python의 strftime 공식 문서](https://docs.python.org/3/library/datetime.html#strftime-and-strptime-behavior)
* [JavaScript의 Date.prototype.toISOString 함수 공식 문서](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Date/toISOString)