---
title:                "새 프로젝트 시작하기"
html_title:           "Haskell: 새 프로젝트 시작하기"
simple_title:         "새 프로젝트 시작하기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
새로운 프로젝트를 시작한다는 것은 무엇인지 말하고 프로그래머들이 왜 이렇게 하는지에 대해 두세 문장으로 설명합니다.

새로운 프로젝트를 시작하는 것은 새로운 소프트웨어나 애플리케이션을 만들기 위해 필요합니다. 프로그래머들은 이를 통해 새로운 문제를 해결하고 그들의 기존 지식과 기술을 발전시키기 위해 노력합니다.

## 하는 법:
```Haskell
main = putStrLn "새로운 프로젝트 시작하기"
```
위와 같은 간단한 코드를 통해 새로운 프로젝트를 시작할 수 있습니다. 이제 여러분의 아이디어를 코드로 구현해보세요.

```Haskell
data Person = Person String Int
getName (Person name age) = name
getAge (Person name age) = age
main = do 
  let me = Person "Jane" 25
  putStrLn $ "제 이름은 " ++ getName me ++ "이고, 나이는 " ++ show (getAge me) ++ "살입니다."
```
위의 예시에서는 `Person`이라는 자료형을 정의하고, `getName`과 `getAge` 함수를 통해 해당 자료형의 값을 추출하고 출력하는 방법을 보여줍니다. 이런식으로 여러분은 여러분만의 자료형과 함수를 정의하여 새로운 프로젝트를 시작할 수 있습니다.

## 더 알아보기:
새로운 프로젝트를 시작하는 것은 프로그래밍의 매우 중요한 부분입니다. 따라서 이를 위한 다양한 방법과 소스 코드 관리 도구, 프로젝트 구조 등에 대해 더 알아보는 것이 공부에 매우 도움이 됩니다.

Haskell 이외에도 JavaScript, Python 등 다른 언어로도 새로운 프로젝트를 시작할 수 있습니다. 하지만 Haskell은 강력한 함수형 프로그래밍 언어이기 때문에 새로운 프로젝트를 시작하는 데 매우 적합합니다.

새로운 프로젝트를 시작할 때에는 단순히 코드를 작성하는 것 외에도 코드의 구조와 사고 과정을 고려하는 것이 중요합니다. 이를 위해 여러분은 다양한 책과 온라인 자료를 참고해 보세요.

## 더 알아볼 만한 것들:
- [Haskell 공식 홈페이지](https://www.haskell.org/)
- [Haskell 공식 튜토리얼](https://www.haskell.org/tutorial/)
- [Haskell로 시작하는 함수형 프로그래밍](https://www.inflearn.com/course/함수형-프로그래밍-haskell)
- [Haskell 배우기: 함수를 이용해 프로그래밍 방식 바꾸기](https://www.edwith.org/functional-haskell)
- [Haskell로 시작하는 고급 함수형 프로그래밍](https://www.inflearn.com/course/고급-함수형-프로그래밍-haskell)

이 외에도 여러분은 구글링을 통해 다양한 자료를 찾아볼 수 있습니다. 새로운 프로젝트를 시작하는 것은 매우 흥미로운 과정이므로, 자극적인 자료를 적극적으로 찾아보고 공부해 보세요!