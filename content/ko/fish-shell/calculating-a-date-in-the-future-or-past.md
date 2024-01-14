---
title:    "Fish Shell: 미래나 과거의 날짜 계산하기"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 왜

날짜를 미래나 과거로 계산하는 것에 참여하는 이유는 무엇일까요? 우리가 대개 우리의 일정을 관리할 때 우리는 날짜에 대한 정보를 필요로 합니다. 때로는 미래의 특정 날짜에 어떤 일이 발생할지 또는 과거에 어떤 일이 발생했는지를 알고 싶을 때도 있습니다. 이러한 경우에는 나중에 설명할 방법으로 Fish Shell 프로그래밍을 사용하여 날짜를 계산할 수 있습니다.

## 어떻게

```Fish Shell``` 코드 블록 내에서 코딩 예제와 샘플 출력을 제공하여 날짜를 계산하는 방법을 보여드리겠습니다. 

### 미래 날짜 계산

우선, 미래 날짜를 계산하려면 ```date``` 함수와 관련된 ```-d``` 옵션을 사용해야 합니다. 아래의 코드를 따라해보세요.

```
date -d "tomorrow"
```

위의 코드를 실행하면 다음 날의 날짜가 출력됩니다. 다음 날이 아니라 더 멀리 떨어진 날짜를 확인하려면 날짜를 포함한 숫자를 입력하면 됩니다. 예를 들어, ```date -d "1 month"```를 입력하면 한 달 후의 날짜가 출력됩니다.

### 과거 날짜 계산

과거 날짜를 계산하는 것은 미래 날짜를 계산하는 것과 유사합니다. 하지만, ```-d``` 옵션 대신에 ```-v``` 옵션을 사용합니다. 아래의 코드를 따라해보세요.

```
date -v -3d
```

위의 코드를 실행하면 3일 전의 날짜가 출력됩니다. 이와 같이 날짜 변수를 조정하고 싶다면 ```-v``` 옵션을 사용하면 됩니다.

## 심층 분석

위에서 보여준 예제들은 날짜를 더 간단하게 계산할 수 있는 방법일 뿐입니다. 그러나 Fish Shell 프로그래밍을 이용하면 더 복잡한 날짜 계산을 수행할 수 있습니다. 예를 들어, 특정 날짜의 다음 주 월요일이나 이전 주 금요일 등을 계산하려면 ```strftime``` 함수를 사용하여 더 세부적인 날짜 정보를 입력할 수 있습니다. 또한, 날짜 정보를 사용하여 반복적인 작업을 자동화하는 스크립트를 작성할 수도 있습니다.

## 관련 링크

- [Fish Shell 공식 홈페이지](https://fishshell.com/)
- [Fish Shell 설치하기](https://fishshell.com/docs/current/setup.html)
- [Fish Shell 기본 사용법](https://fishshell.com/docs/current/tutorial.html)
- [Fish Shell 사용자 지침서](https://fishshell.com/docs/current/index.html)