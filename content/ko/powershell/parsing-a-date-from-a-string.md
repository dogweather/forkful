---
title:                "문자열에서 날짜 파싱하기"
html_title:           "PowerShell: 문자열에서 날짜 파싱하기"
simple_title:         "문자열에서 날짜 파싱하기"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

일반적으로 프로그래밍에서는 날짜나 시간 정보를 컴퓨터가 이해할 수 있는 데이터 형식으로 변환하는 작업이 필요합니다. 이를 "문자열에서 날짜 파싱"이라고 합니다. 프로그래머들은 이 작업을 하는 이유는, 데이터를 처리하거나 특정 기능을 구현하기 위해서는 정확한 날짜 또는 시간 정보가 필요하기 때문입니다.

## 방법:

```PowerShell
# 문자열에서 날짜 파싱하기
$dateString = "2020-05-20"
$date = [DateTime]::Parse($dateString)
# $date 변수에 2020년 5월 20일의 날짜 정보가 저장됩니다.
# ToString() 메서드를 사용하여 날짜 형식을 원하는 대로 변경할 수 있습니다.
$date.ToString("dd/MM/yyyy")
```

**결과:**
20/05/2020

## 깊이 파고들기:

(1) "문자열에서 날짜 파싱"은 컴퓨터가 발달하기 이전부터 사용되어왔습니다. 예를 들어, 고대 로마 시대에는 날짜를 표현하기 위해 다른 문자나 기호를 사용했습니다.
(2) PowerShell에서는 [DateTime] 클래스를 통해 날짜와 시간 정보를 다룰 수 있습니다. 또는, 일반적인 문자열 함수를 사용해서도 날짜를 파싱할 수 있습니다.
(3) 날짜 형식이 다양하기 때문에, 일반적으로 파싱 알고리즘은 복잡한 작업입니다. 따라서 이를 위해 표준 라이브러리에서 제공하는 기능을 사용하는 것이 좋습니다.

## 더 알아보기:

- [PowerShell 디렉토리](https://docs.microsoft.com/ko-kr/powershell/)
- [Microsoft 도구](https://www.microsoft.com/ko-kr/)