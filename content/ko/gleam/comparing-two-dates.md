---
title:    "Gleam: 두 날짜 비교하기"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

Gleam 프로그래밍 블로그 게시물 - 두 날짜 비교하기

## 왜 하나요?
두 날짜를 비교하려면 어떻게 해야 할까요? 파이썬에서는 날짜 2개를 비교하는 까다로운 작업입니다. 그래서 Gleam에서는 이 과정을 더 간단하게 만들어줍니다. 이 블로그 게시물에서는 왜 두 날짜를 비교해야 하는지 알려드리고, Gleam을 사용해서 어떻게 간단하게 비교할 수 있는지 알려드리겠습니다.

## 어떻게 하나요?
Gleam에서는 두 날짜를 비교하는 `Date.compare` 함수를 제공합니다. 이 함수는 첫 번째 인자에는 비교할 날짜를, 두 번째 인자에는 기준 날짜를 넣어주면 됩니다. 두 날짜가 서로 같은지, 앞서는지 혹은 뒤에 있는지 비교할 수 있습니다.

```Gleam
let result = Date.compare(#time(2021, 10, 10), #time(2021, 10, 11))
```

위 코드의 결과는 `-1`이 나올 것입니다. 이는 첫 번째 인자인 `#time(2021, 10, 10)`이 기준 날짜 `#time(2021, 10, 11)`보다 앞서 있음을 나타냅니다.

또 다른 예시를 살펴볼까요?

```Gleam
let result = Date.compare(#time(2021, 10, 11), #time(2021, 10, 10))
```

이번에는 `1`이 나올 것입니다. 첫 번째 인자인 `#time(2021, 10, 11)`이 기준 날짜 `#time(2021, 10, 10)`보다 뒤에 있음을 나타내죠.

위와 같은 방법으로 두 날짜를 비교할 수 있습니다. 이렇게 간단하게 두 날짜를 비교할 수 있다는 것은 많은 시간과 노력을 아낄 수 있다는 점에서 매우 유용합니다.

## 딥 다이브
Gleam에서는 `Date.compare` 함수를 뿐만 아니라 다양한 날짜 관련 함수를 제공합니다. 이 함수들을 잘 활용하면 복잡한 날짜 연산도 손쉽게 처리할 수 있습니다. 자세한 내용은 공식 문서를 참고해주세요.

## 관련 자료
[Gleam 공식 문서](https://gleam.run/)  
[Gleam GitHub 저장소](https://github.com/gleam-lang)  
[파이썬으로 두 날짜 비교하기](http://blog.naver.com/example)