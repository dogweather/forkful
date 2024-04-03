---
date: 2024-01-20 17:36:41.924981-07:00
description: "\uB0A0\uC9DC\uB97C \uBB38\uC790\uC5F4\uB85C \uBCC0\uD658\uD55C\uB2E4\
  \uB294 \uAC74 \uD2B9\uC815 \uB0A0\uC9DC\uB97C \uD14D\uC2A4\uD2B8 \uD615\uC2DD\uC73C\
  \uB85C \uBC14\uAFB8\uB294 \uAC83\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\
  \uB4E4\uC740 \uBA85\uD655\uD55C \uAE30\uB85D, \uC0AC\uC6A9\uC790 \uCE5C\uD654\uC801\
  \ \uD45C\uC2DC, \uB370\uC774\uD130 \uD3EC\uB9F7 \uC870\uC815 \uB4F1\uC744 \uC704\
  \uD574 \uC774 \uC791\uC5C5\uC744 \uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.872677-06:00'
model: gpt-4-1106-preview
summary: "\uB0A0\uC9DC\uB97C \uBB38\uC790\uC5F4\uB85C \uBCC0\uD658\uD55C\uB2E4\uB294\
  \ \uAC74 \uD2B9\uC815 \uB0A0\uC9DC\uB97C \uD14D\uC2A4\uD2B8 \uD615\uC2DD\uC73C\uB85C\
  \ \uBC14\uAFB8\uB294 \uAC83\uC785\uB2C8\uB2E4."
title: "\uB0A0\uC9DC\uB97C \uBB38\uC790\uC5F4\uB85C \uBCC0\uD658\uD558\uAE30"
weight: 28
---

## How to: (방법)
Fish 쉘에서 날짜를 문자열로 변환하는 두 가지 방법을 보여드리겠습니다.

### 방법 1: date 명령어 사용
```Fish Shell
set date_string (date "+%Y-%m-%d")
echo $date_string
```

##### 출력
```
2023-03-15
```

### 방법 2: strftime 함수 사용
```Fish Shell
set date_string (strftime "%Y-%m-%d %H:%M:%S" (date +%s))
echo $date_string
```

### 방법 2: strftime 함수 사용
```Fish Shell
set date_string (strftime "%Y-%m-%d %H:%M:%S" (date +%s))
echo $date_string
```

## Deep Dive (심층 탐구)
날짜를 문자열로 변환하는 것은 Unix 시대부터 있었던 기능입니다. Unix의 'date' 명령어나 C 프로그래밍 언어의 'strftime' 함수에서 기원을 찾을 수 있죠.

Fish Shell에서 'date' 명령어를 사용하면 유닉스 'date' 기능을 직접 활용할 수 있습니다. '+%Y-%m-%d' 같은 형식 지정자를 통해 다양한 포맷을 만들 수 있죠.

'strftime' 함수는 더욱 유연합니다. 시간을 초로만 나타내는 Unix timestamp를 가지고 인간이 읽을 수 있는 형태로 변환합니다. Fish Shell에서는 'strftime'를 활용하여 좀 더 복잡한 날짜 형식도 쉽게 다룰 수 있습니다.

위 두 방법 외에도, 타 프로그래밍 언어나 시스템 자체의 기능을 가져와 활용하는 방법도 있습니다. 예를 들어, 'python'이나 'awk' 같은 프로그래밍 언어를 사용하기도 하죠.

## See Also (참고 자료)
- Fish Shell 공식 문서: https://fishshell.com/docs/current/index.html
- Date 명령어 매뉴얼: https://linux.die.net/man/1/date
- strftime 함수에 대한 정보: https://en.wikipedia.org/wiki/Strftime
