---
title:                "Bash: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 왜

현재 날짜를 얻는 방법을 배우는 것은 Bash 프로그래밍에 있어서 굉장히 중요합니다. 현재 날짜는 스크립트를 작성하거나 파일을 생성할 때 유용하게 사용될 수 있습니다.

## 어떻게

현재 날짜를 얻는 방법에는 여러 가지가 있지만, 가장 간단하게는 `date` 명령어를 사용하는 것입니다. 예를 들어, 다음과 같이 입력하면 현재 날짜와 시간이 출력됩니다:

```Bash
date
```

출력 예시:

```Bash
Sun Oct 31 18:35:21 KST 2021
```

또 다른 방법은 `date` 명령어에 옵션을 추가하여 출력 형식을 지정하는 것입니다. 예를 들어, `+%m-%d-%Y` 옵션을 사용하면 현재 날짜를 월-일-년 형식으로 출력할 수 있습니다:

```Bash
date +%m-%d-%Y
```

출력 예시:

```Bash
10-31-2021
```

위와 같이 옵션을 조합하여 원하는 형식으로 날짜를 출력할 수 있습니다. 또한 `date` 명령어를 변수에 할당하여 스크립트에서 사용할 수도 있습니다:

```Bash
current_date=$(date +%Y-%m-%d)
echo "오늘의 날짜는 ${current_date}입니다."
```

출력 예시:

```Bash
오늘의 날짜는 2021-10-31입니다.
```

## 깊게 파고들기

`date` 명령어에 대해 더 자세하게 알고 싶다면 `man date` 명령어를 사용하여 매뉴얼 페이지를 확인할 수 있습니다. 여기서는 명령어의 사용법뿐만 아니라 다양한 옵션과 예제에 대한 정보를 얻을 수 있습니다.

Bash 스크립트에서 `date` 명령어를 사용할 때 주의할 점은 지역 설정이 영향을 줄 수 있다는 것입니다. 즉, 날짜와 시간 형식이 지역에 따라 다를 수 있다는 것을 의미합니다. 이를 해결하기 위해 `LC_TIME` 환경 변수를 사용하여 지역 설정을 변경할 수 있습니다. 또한 `TZ` 환경 변수를 사용하여 원하는 시간대의 날짜와 시간을 출력할 수도 있습니다.

## 더 알아보기

- ["bash scripting tutorial"](https://ryanstutorials.net/bash-scripting-tutorial/) - Bash 스크립트 작성 방법에 대한 자세한 튜토리얼입니다.
- ["10 Essential Tips for Writing Better Bash Scripts"](https://dev.to/drakulavich/10-essential-tips-for-writing-better-bash-scripts-57k6) - Bash 스크립트 작성 시 유용한 팁을 알려주는 블로그 포스트입니다.
- ["date command in Linux with examples"](https://www.geeksforgeeks.org/date-command-in-linux-with-examples/) - 다양한 `date` 명령어의 예제를 제공하는 매우 유용한 레퍼런스입니다.

## 더 알아보기