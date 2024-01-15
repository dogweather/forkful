---
title:                "날짜를 문자열로 변환하기"
html_title:           "Fish Shell: 날짜를 문자열로 변환하기"
simple_title:         "날짜를 문자열로 변환하기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 왜

만약 당신이 Fish Shell 프로그래밍의 팬이라면, 날짜를 문자열로 변환하는 것을 해보고 싶을 수 있을 것입니다. 이 기능을 사용하면, 날짜를 사용하기 쉽고 효율적인 방식으로 표현할 수 있습니다.

## 하는 방법

Fish Shell의 ```date``` 함수를 사용하여 문자열로 변환할 날짜를 지정하세요. 예를 들어, 만약 오늘의 날짜를 문자열로 변환하고 싶다면, 다음과 같이 작성하세요:

```
Fish Shell> date '%m-%d-%Y'
```

위의 코드는 현재 날짜를 "월-일-년" 형식의 문자열로 반환할 것입니다. 출력은 다음과 같을 것입니다:

```
03-16-2021
```

날짜 문자열을 원하는 형식대로 바꾸고 싶다면, 별도의 매개변수를 추가할 수 있습니다. 예를 들어, "월 일, 년" 형식으로 변환하고 싶다면, 다음과 같이 작성합니다:

```
Fish Shell> date '+%B %d, %Y'
```

위의 코드는 "3월 16, 2021"과 같은 형식의 문자열을 반환할 것입니다. 참고로, ```%B```는 월의 이름을, ```%d```는 날짜를, 그리고 ```%Y```는 년도를 의미합니다.

## 자세히 살펴보기

Fish Shell의 날짜 함수는 다양한 매개변수를 제공합니다. 아래는 일부 매개변수와 그 기능들을 간략하게 소개합니다:

- ```%Y```: 네 자리의 년도를 반환합니다.
- ```%y```: 두 자리의 년도를 반환합니다.
- ```%m```: 두 자리의 월을 반환합니다 (01 ~ 12).
- ```%B```: 전체 월의 이름을 반환합니다.
- ```%b```: 축약된 월의 이름을 반환합니다.
- ```%d```: 두 자리의 날짜를 반환합니다 (01 ~ 31).
- ```%e```: 공백이 추가된 두 자리의 날짜를 반환합니다 (1 ~ 31).
- ```%p```: 오전/오후를 나타내는 문자열을 반환합니다.
- ```%H```: 24시간 형식의 시간을 반환합니다.
- ```%I```: 12시간 형식의 시간을 반환합니다.
- ```%M```: 두 자리의 분을 반환합니다 (00 ~ 59).
- ```%S```: 두 자리의 초를 반환합니다 (00 ~ 59).

더 많은 옵션들을 확인하고 싶다면, [Fish Shell 공식 문서](https://fishshell.com/docs/current/cmds/date.html)를 참고해주세요.

## 더 읽어보기

여러분의 Fish Shell 프로그래밍 실력을 향상시키기 위해, 아래 링크들을 더 읽어보세요:

- [Fish Shell 공식 문서](https://fishshell.com/docs/current/index.html)
- [최신 Fish Shell 업데이트 정보](https://fishshell.com/release_notes/current.html)
- [Fish Shell의 효율적인 사용법](https://healthysoftwaredeveloper.com/efficient-fish-shell/)
- [Fish Shell과 Zsh의 차이점](https://healthysoftwaredeveloper.com/fish-use-differences-zsh/)

## 관련 링크들

- [Fish Shell 공식 홈페이지](https://fishshell.com/)
- [Fish Shell GitHub 저장소](https://github.com/fish-shell/fish-shell)
- [Fish Shell 레포지토리 및 매뉴얼 번역 프로젝트