---
title:    "Bash: 현재 날짜 가져오기"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 왜
현재 날짜를 알아내는 것에 관심을 가져야 할까요? 이것은 컴퓨터 프로그램을 작성하거나 빅 데이터 관리하는 등의 작업에서 유용한 기능입니다. 또한 일상 생활에서도 날짜 정보를 필요로 할 때가 많습니다. 이 방법은 여러분이 언제 어떤 작업을 한 것인지를 알 수 있게 해주는 중요한 도구입니다.

## 사용 방법
현재 날짜를 받아오는 방법은 매우 간단합니다. 아래의 샘플 코드를 참고하세요.

```Bash
# 현재 날짜를 "년-월-일" 형식으로 얻기
date '+%Y-%m-%d'
```

위의 코드를 실행하면 현재 날짜가 "년-월-일" 형식으로 출력됩니다. 예를 들어, 오늘 날짜가 2021년 10월 26일일 경우 `2021-10-26`이 출력됩니다.

코드를 조금 더 수정하여 특정 포맷으로 날짜를 출력할 수도 있습니다. 아래의 예시 코드를 참고하세요.

```Bash
# 현재 날짜를 "월/일/년 시분초" 형식으로 얻기
date '+%m/%d/%Y %H%M%S'
```

위의 코드를 실행하면 현재 날짜와 시간이 "월/일/년 시분초" 형식으로 출력됩니다. 예를 들어, 현재 시간이 오전 11시 30분 20초일 경우 `10/26/2021 113020`이 출력됩니다.

더 많은 날짜 포맷에 대해서는 `man date` 명령어를 사용하거나 인터넷에서 검색하여 확인할 수 있습니다.

## 깊이 있는 분석
현재 날짜를 얻는 방법의 원리는 시스템의 하드웨어 클럭 값을 사용하는 것입니다. 이 클럭 값은 시계 전자 회로에서 생성되며 소프트웨어에서 이 값을 읽어서 현재 날짜와 시간을 계산합니다.

또한 시스템의 타임존 설정에 따라 현재 날짜와 시간이 달라질 수 있습니다. 서울 시간대로 타임존이 설정되어 있는 경우 현재 날짜와 시간은 한국 표준시로 출력됩니다.

## 또 다른 참고 자료
- [Bash에서 날짜를 포맷하기](https://www.computerhope.com/unix/udate.htm)
- [날짜를 포맷하는 다양한 방법](https://www.tecmint.com/bash-format-date-and-time-in-linux/)
- [쉘 스크립트에서 시간과 날짜 다루기](https://www.cyberciti.biz/tips/linux-unix-formatting-dates-for-display.html)

## 참고 자료
[See Also]:가장 최신의 외부 링크
- [Linux에서 날짜와 시간 다루기](https://www.redhat.com/sysadmin/date-and-time-commands-linux)