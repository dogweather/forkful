---
title:    "Fish Shell: 현재 날짜 가져오기"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## 왜
현재 날짜를 구하는 것에 대해 관심이 있을까요? 일반적으로 컴퓨터 프로그래밍에서 현재 날짜를 사용하는 이유는 다양합니다. 일정한 작업을 스케줄링하거나 시간 기반 이벤트를 처리하거나, 간단하게 현재 날짜를 표시하고 싶은 경우 등등 많은 이유가 있습니다.

## 어떻게
어떻게 Fish Shell에서 현재 날짜를 제공할 수 있을까요? 다행히도 Fish Shell은 매우 간단한 명령으로 현재 날짜를 출력해줍니다. 아래의 코드를 터미널에서 실행해보세요.

```Fish Shell
date
```

이렇게 하면 현재 날짜와 시간이 출력될 것입니다. 예를 들어, "2021년 8월 11일 수요일 오후 2시 30분"과 같은 형식으로 출력됩니다. 만약 다른 형식으로 날짜를 출력하고 싶다면 `--format` 옵션을 사용할 수 있습니다. 예를 들어 다음과 같이 입력하면 "21/08/11"과 같은 형식으로 날짜가 출력됩니다.

```Fish Shell
date --format=%y/%m/%d
```

## 딥 다이브
지금까지는 Fish Shell의 `date` 명령어를 사용해서 현재 날짜를 출력하는 것에 대해 알아보았습니다. 하지만 더 깊이 들어가서 현재 날짜가 어떻게 제공되는지 알아보겠습니다.

Fish Shell에서 `date` 명령어는 내부적으로 `date` 함수를 호출합니다. 따라서 Fish Shell의 소스 코드를 살펴보면 `date` 함수가 어떻게 구현되어 있는지 알 수 있습니다. 또한 `man` 명령어를 사용해서 `date` 함수의 매뉴얼 페이지를 확인할 수도 있습니다.

## 또 보기
현재 날짜를 제공하는 것 외에도 Fish Shell의 다양한 명령어를 알고 싶다면 아래의 링크들을 참고해보세요.

- [Fish Shell 공식 문서](https://fishshell.com/docs/)
- [Fish Shell Github 저장소](https://github.com/fish-shell/fish-shell)
- [Fish Shell 사용 팁과 트릭](https://medium.com/@olesk75/10-tips-for-using-fish-shell-1a6fdb81dd06)