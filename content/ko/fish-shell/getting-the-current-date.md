---
title:    "Fish Shell: 현재 날짜 가져오기"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

저희가 모두 알고있듯이 오늘 날짜를 알고있는 것은 매우 유용합니다. 불과 몇 년 전까지는 달력을 뒤져야 했지만, 우리는 이제 컴퓨터를 켜고 **Fish Shell**을 실행하면 즉시 오늘 날짜를 바로 확인할 수 있습니다. 이런 작업들을 더 편리하게 만들기 위해, 오늘은 **Fish Shell**에서 현재 날짜를 가져오는 방법에 대해 알아보겠습니다.

## 왜
오늘 날짜를 가져오는 것은 매우 유용합니다. 예를 들어, 우리가 마감 날짜를 맞추기 위해 중요한 프로젝트에 참여하고 있을 때, 현재 날짜를 알고있다면 몇 일 남았는지 쉽게 계산할 수 있습니다. 또는 생각해보세요, 작업 중인 파일의 이름에 자동으로 오늘 날짜를 추가한다면 얼마나 좋을까요? 그렇기 때문에 현재 날짜를 가져오는 것은 프로그래밍에서 매우 유용합니다.

## 사용 방법
우선, **Fish Shell**을 실행하세요. 그리고 아래의 코드를 복사하여 붙여넣기 한 후 엔터를 누르세요.

```Fish Shell
set today (date +%Y-%m-%d)
echo $today
```

그러면 화면에 현재 날짜가 정확한 형식으로 출력될 것입니다. 예를 들어, 오늘 날짜가 2021년 10월 15일이라면 "2021-10-15"라는 결과가 나올 것입니다.

## 딥 다이브
그렇다면 위의 코드를 좀 더 자세히 살펴보겠습니다. 첫 번째 줄의 `set today`는 변수를 설정하는 구문입니다. 그리고 `date +%Y-%m-%d`는 현재 날짜를 가져오는 명령어입니다. 여기서 `%Y`는 년도, `%m`은 월, `%d`는 일을 나타냅니다. 따라서 위의 예시에서 "2021-10-15" 중 "2021"은 `%Y`, "10"은 `%m`, "15"는 `%d`에 해당합니다. 마지막으로 `echo $today`는 변수에 저장된 값을 출력하는 구문입니다. 따라서 위의 예시에서 오늘 날짜가 저장된 변수를 출력하므로 "2021-10-15"가 출력된 것입니다.

## 더 알아보기
오늘 날짜를 가져오는 더 많은 방법들이 있는데, 그 중에는 `date` 명령어의 다양한 옵션들을 활용하는 것도 있습니다. 더 알아보고 싶다면 아래의 링크들을 참고해보세요.

## 관련 링크
- [Fish Shell 공식 문서](https://fishshell.com/docs/current/cmds/date.html)
- [Linux 명령어 - date](https://zetawiki.com/wiki/%EB%A6%AC%EB%88%85%EC%8A%A4_%EB%AA%85%EB%A0%B9%EC%96%B4_-_%EB%8B%A4%ED%8A%B8)
- [Date and time formatting in Fish Shell](https://thoughtbot.com/blog/date-and-time-formatting-in-fish-shell)

## 참고
모두 함께 즐겁게 **Fish Shell** 프로그래밍을 배워보세요! 그리고 항상 최신 날짜를 이용하여 유용한