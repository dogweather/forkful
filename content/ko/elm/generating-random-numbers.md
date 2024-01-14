---
title:                "Elm: 랜덤 숫자 생성하기"
programming_language: "Elm"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 왜
랜덤 숫자 생성에 참여하는 이유는 주로 게임이나 시뮬레이션 등의 소프트웨어에서 다양한 상황을 구현하기 위해서입니다. 또한 보안, 데이터 분석 및 암호화 등에도 활용될 수 있습니다.

## 하는 법
```Elm
-- 1부터 10까지의 랜덤 정수 생성
Random.int 1 10

-- 주어진 리스트에서 랜덤 요소 선택
Random.oneOf ["사과", "배", "딸기"]

-- 확률을 지정하여 랜덤 선택
Random.weighted [ (1, "검정색"), (2, "하얀색"), (3, "빨간색") ]

-- 랜덤 문자열 생성
Random.custom (RandomList.fromList ["A", "B", "C"]) (Random.name "ABCDE")

-- 랜덤 난수 생성 
Random.float 0 1
```

위와 같이 ```Random``` 라이브러리를 사용하여 쉽게 랜덤 숫자를 생성할 수 있습니다. 예를 들어, 주사위 굴리기 게임을 구현할 때, 주사위의 눈금을 랜덤으로 생성하여 다양한 결과를 얻을 수 있습니다.

## 깊이 파고들기
```Random``` 라이브러리는 난수 생성을 위한 다양한 기능을 제공합니다. 더 정교한 난수 생성을 위해 ```Random.Generator```를 사용할 수 있습니다. 또한, ```Random.Step```과 같은 함수를 사용하여 난수 생성을 더욱 세밀하게 조정할 수 있습니다. 이 외에도 랜덤 숫자 생성에 관련된 다양한 정보를 알아보고, 더 많은 기능을 적용해보세요.

## 참고 자료
- [Elm 공식 랜덤 문서](https://guide.elm-lang.org/effects/random.html) 
- [Elm Random 라이브러리 소개](https://blog.naver.com/runner0718/222261758909)
- [Elm으로 주사위 굴리기 구현해보기](https://hoomal.com/dev/elm/2021/01/24/rolling-dice-elm.html)