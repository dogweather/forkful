---
date: 2024-01-20 17:36:41.924981-07:00
description: "How to: (\uBC29\uBC95) Fish \uC258\uC5D0\uC11C \uB0A0\uC9DC\uB97C \uBB38\
  \uC790\uC5F4\uB85C \uBCC0\uD658\uD558\uB294 \uB450 \uAC00\uC9C0 \uBC29\uBC95\uC744\
  \ \uBCF4\uC5EC\uB4DC\uB9AC\uACA0\uC2B5\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:57.458795-06:00'
model: gpt-4-1106-preview
summary: "(\uBC29\uBC95) Fish \uC258\uC5D0\uC11C \uB0A0\uC9DC\uB97C \uBB38\uC790\uC5F4\
  \uB85C \uBCC0\uD658\uD558\uB294 \uB450 \uAC00\uC9C0 \uBC29\uBC95\uC744 \uBCF4\uC5EC\
  \uB4DC\uB9AC\uACA0\uC2B5\uB2C8\uB2E4."
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
