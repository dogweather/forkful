---
title:                "Fish Shell: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

# 왜

현재 날짜를 가져오는 법을 배우면, 시스템에서 사용하는 날짜와 시간을 간편하게 확인할 수 있습니다.

## 어떻게

먼저, Fish Shell에서 "date" 명령어를 사용하여 현재 날짜를 가져올 수 있습니다. 아래의 코드 블럭을 참고해주세요.

```Fish Shell
date
```

출력 결과는 현재 시스템 날짜와 시간이 나타납니다. 예를 들어,

```Fish Shell
Mon Aug 30 21:14:31 KST 2021
```

만약 날짜와 시간의 형식을 변경하고 싶다면, "date" 명령어 다음에 형식을 지정해주면 됩니다. 예를 들어,

```Fish Shell
date +"%Y/%m/%d %H:%M:%S"
```

라는 코드를 입력하면, 날짜와 시간이 다음과 같은 형식으로 나옵니다.

```Fish Shell
2021/08/30 21:14:31
```

더 많은 형식 옵션은 Fish Shell 공식 문서를 참고해주세요.

## 깊이 파고들기

날짜와 시간 형식을 지정하는 방법 외에도 여러 가지 옵션이 있습니다. 예를 들면, 특정 날짜와 시간을 설정하는 방법이나, 다른 시간대의 시간을 확인하는 방법 등이 있습니다. 아래의 링크들을 참고하시면 더 자세한 내용을 알 수 있습니다.

# 참고 자료

- [Fish Shell 공식 문서](https://fishshell.com/docs/current/commands.html#date)
- [날짜와 시간 형식 옵션](https://fishshell.com/docs/current/cmds/date.html#formatting-date-and-time)