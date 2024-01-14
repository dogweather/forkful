---
title:                "Bash: 현재 날짜 얻기"
programming_language: "Bash"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 왜 
오늘 날짜를 얻는 작업에 참여하는 이유는 무엇일까요?

## 어떻게 하나요
오늘 날짜를 얻는 방법에 대해 알아보겠습니다. 
```Bash
# 현재 날짜를 출력하는 예제 코드 
# date command를 사용합니다. 
date 
```

위의 예제 코드를 실행하면 다음과 같은 결과가 출력됩니다. 

```Bash 
Thu Feb 20 16:02:32 EST 2020
```

이제 간단한 Bash 스크립트를 작성해보겠습니다. 
```Bash
#!/bin/bash 
# 현재 날짜를 변수에 저장하는 스크립트 
CURRENT_DATE=$(date)
echo "오늘 날짜는 $CURRENT_DATE 입니다."
```

위의 스크립트를 실행하면 다음과 같은 결과가 출력됩니다. 

```Bash 
오늘 날짜는 Thu Feb 20 16:02:32 EST 2020 입니다.
```

이제 날짜를 다양한 형식으로 출력하는 방법을 살펴보겠습니다. 

```Bash 
# 현재 날짜를 원하는 형식으로 출력하는 예제 코드 
date '+오늘은 %Y년 %m월 %d일입니다.'
```

위의 예제 코드를 실행하면 다음과 같은 결과가 출력됩니다. 

```Bash 
오늘은 2020년 02월 20일입니다. 
```

더 많은 형식을 사용하고 싶다면 Bash의 man 페이지에서 date command를 확인해보세요. 

## Deep Dive 
date command는 운영체제에 설치된 환경 변수와 통합하여 날짜와 시간을 출력합니다. 이는 Linux의 시스템 시계와 동기화되어 정확도가 높다는 장점이 있습니다. 또한 date command는 UTC(Universal Time Coordinated)를 기준으로 날짜와 시간을 표시하며 시간대를 포함한 다양한 옵션을 제공합니다. 

추가적으로 date command는 현재 시간이 아닌 과거나 미래의 특정 날짜를 출력할 수도 있습니다. 자세한 내용은 man 페이지를 확인해보세요. 

## See Also
- [Bash Reference Manual](https://www.gnu.org/software/bash/manual/html_node/index.html)
- [Linux man Pages](https://www.kernel.org/doc/man-pages/)
- [Linux Commands Cheat Sheet](https://cheatography.com/davechild/cheat-sheets/linux-command-line/)