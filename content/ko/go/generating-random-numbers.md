---
title:                "난수 생성하기"
html_title:           "Go: 난수 생성하기"
simple_title:         "난수 생성하기"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 무엇인고, 왜?
랜덤 숫자 생성이 무슨 일인지는 꽤 명확해요. 우리가 컴퓨터 프로그래밍을 할 때, 간혹 필요할 때가 있어요. 일반적인 예를 들어, 우리는 랜덤 숫자를 사용하여 게임에서 다양한 상황이 될 수 있도록 합니다. 즉, 이것은 우리가 미리 정해놓은 결과가 아니라, 각각의 시도마다 다른 결과를 얻을 수 있도록 해줘요. 

## 하는 법:
Go에서는 매우 간단하게 랜덤 숫자를 생성할 수 있어요. 다음 예시를 보세요:
```Go
// 1부터 100까지의 범위에서 랜덤 숫자 생성
go 랜덤 숫자 := rand.Intn(100)
fmt.Println(랜덤 숫자)

// 0부터 99까지의 범위에서 랜덤 숫자 생성
go 랜덤 숫자 := rand.Intn(100)
fmt.Println(랜덤 숫자)
```
 
## 깊이 파헤쳐보기:
랜덤 숫자 생성은 많은 언어에서 이미 예전부터 지원되어왔어요. 그래서 우리는 당연히 포함되어 있는 것이라고 생각할 수 있지만, 실제로는 쉽지 않은 배경이 있어요. 랜덤 숫자 생성은 컴퓨터에서 진행하는 계산에 대한 예측 가능성을 손상시키려는 시도이기도 해요. 이것은 암호화와 관련이 있고, 많은 노력이 들어가는 분야이기도 해요. 하지만 다행히도 우리는 Go에서 이것을 간단하게 처리할 수 있어요. 또한, 우리는 crypto/rand 패키지를 사용하여 보안적으로 더 안전한 랜덤 수를 생성할 수 있다는 것도 알아두세요.

## 참고해봐요:
Go 공식 문서에서 더 많은 정보를 얻을 수 있어요. 또한, 랜덤 숫자 생성과 별개로, 우리는 이것을 어떻게 활용할 수 있는지에 대한 아이디어를 찾아볼 수도 있어요. 더 많은 정보와 예시를 보시려면, 아래 자료들을 참고하세요.

- [Go 공식 문서](https://golang.org/pkg/math/rand/)
- [Bitcoin 암호화 기술에서의 난수 생성](https://arxiv.org/pdf/1112.3076.pdf)
- [다양한 용도로 활용할 수 있는 랜덤 숫자 생성의 유용함](https://medium.com/@sfbehnke/why-random-numbers-are-useful-and-how-to-use-them-8d78cb6c947b)