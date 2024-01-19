---
title:                "문자열에서 날짜 분석하기"
html_title:           "Gleam: 문자열에서 날짜 분석하기"
simple_title:         "문자열에서 날짜 분석하기"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 무엇이며 왜합니까?
문자열에서 날짜를 파싱한다는 것은 복잡한 문자열에서 특정 날짜 정보를 추출하는 것을 의미합니다. 프로그래머들은 날짜정보가 새겨진 로그 파일처럼 날짜정보가 기록된 텍스트 데이터를 처리할 때 이를 사용합니다.

## 방법:
아래의 Bash 코드를 보면 문자열에서 날짜를 파싱하는 방법을 확인할 수 있습니다. 긴 문자열에서 날짜를 추출하여 '$date'변수에 저장합니다.

```Bash
string="2021년 10월 5일, 화요일, 임의의 문자열"
date=`echo $string | grep -oP '\d{4}년 \d{1,2}월 \d{1,2}일'`
echo $date
```

위 코드를 실행할 경우 아래와 같은 출력값이 나옵니다:

```Bash
2021년 10월 5일
```

## 심층 힐끔:
날짜 파싱은 사실 프로그래밍의 역사와 함께한 오래된 기술입니다. 이는 단지 문자열 에서 날짜 정보만을 추출하는 것이 아니라, 파싱 된 날짜 정보를 사용하여 복잡한 날짜 계산과 같은 다양한 작업에 활용하기 위함이었습니다. 하지만 유의할 점은 파싱을 사용하는 예제 코드에서 보이는 바와 같이 원하는 패턴에 따라 파싱을 수행해야 한다는 것입니다.

Bash에서는 위에서 보여준 예제와 같이 'grep'와 'regex'를 활용하여 파싱을 수행하지만, 다른 언어에서는 더욱 강력한 라이브러리를 사용하여 파싱을 수행할 수 있습니다. Perl의 경우 'Time::Piece' 라이브러리, Python에서는 'dateutil.parser' 라이브러리 같은 도구를 사용하여 검색할 수 있습니다.

## 참고 자료:
* Bash script에서 날짜 파싱을 내가 원하던 형식으로 파싱하는 방법: [link](https://stackoverflow.com/questions/23317504/how-to-parse-and-break-down-a-date-string-in-bash)
* Perl과 Python에서의 날짜 파싱:
  * Perl: [link](https://perldoc.perl.org/Time/Piece.html)
  * Python: [link](https://dateutil.readthedocs.io/en/stable/parser.html)