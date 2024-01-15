---
title:                "현재 날짜 얻기"
html_title:           "Fish Shell: 현재 날짜 얻기"
simple_title:         "현재 날짜 얻기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 왜

현재 날짜를 가져오는 이유는 간단합니다 - 우리는 일상 생활에서 자주 현재 날짜를 필요로하기 때문입니다. 일정을 만들고 기록하며 마감일을 추적하는 등 다양한 상황에서 현재 날짜를 알아야 할 수 있습니다.

## 방법

```Fish Shell```에서 현재 날짜를 가져오는 방법은 매우 간단합니다. 터미널에서 ```date``` 명령어를 입력하면 현재 날짜와 시간이 표시됩니다.

```fish shell
date
```

출력 예시:

```shell
Wed Feb 19 09:23:07 PST 2020
```

또한, ```Fish Shell```에서는 ```date``` 명령어에 옵션을 추가해 더 다양한 형식의 날짜를 가져올 수 있습니다. 예를 들어, ```+%m-%d-%Y``` 옵션을 추가하면 현재 날짜를 월-일-년도의 형식으로 가져올 수 있습니다.

```fish shell
date +%m-%d-%Y
```

출력 예시:

```shell
02-19-2020
```

## 심도 들어가기

```date``` 명령어는 시스템의 현재 날짜와 시간을 확인하는 데 사용되는 간단한 명령어입니다. 이 명령어는 시스템의 기본 설정에 따라 다양한 옵션을 가질 수 있으며, 사용자의 요구에 따라 다양한 날짜 형식을 제공할 수 있습니다.

또한, ```date``` 명령어는 시스템의 시간대를 변경하는 데에도 사용될 수 있습니다. 시간대를 변경하면 시스템의 현재 날짜와 시간이 변경되며, 다른 지역의 날짜와 시간을 확인할 수 있습니다.

## 더 알아보기

- [Fish Shell 공식 홈페이지](https://fishshell.com/)
- [Fish Shell 사용법 안내서](https://fishshell.com/docs/current/index.html)
- [Fish Shell 공식 GitHub 저장소](https://github.com/fish-shell/fish-shell)

## 참고 자료

- [Linuxize: How to Get Current Date and Time in Linux](https://linuxize.com/post/how-to-get-current-date-and-time-in-linux/)
- [nixCraft: How to Write the Current Date and Time to a File in a Shell Script](https://www.cyberciti.biz/faq/linux-unix-write-simple-current-date-time-into-logfile/)