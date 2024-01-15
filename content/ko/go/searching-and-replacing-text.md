---
title:                "텍스트 검색 및 교체"
html_title:           "Go: 텍스트 검색 및 교체"
simple_title:         "텍스트 검색 및 교체"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 왜

어떤 이유로 당신은 텍스트를 검색하고 대체하는 작업에 관심이 있을까요? 한 마디로 말하면, 이 작업은 매우 유용합니다. 특히 큰 규모의 코드베이스를 가지고 작업할 때 특히 더 그렇습니다. 검색하고 대체하는 기능을 사용하면 원하는 내용을 찾아서 빠르게 수정해줄 수 있고, 작업 시간을 줄일 수 있습니다.

## 어떻게

자, 그럼 검색하고 대체하는 방법에 대해 알아보겠습니다. Go 언어에서는 텍스트를 검색하고 대체하는데 사용할 수 있는 내장 함수들이 있습니다. 가장 기본적인 함수는 `strings.Replace()` 함수입니다. 이 함수는 세 개의 인자를 전달받으며, 첫 번째 인자에는 대상 문자열을 전달하고, 두 번째 인자에는 대체할 내용을 전달하고, 세 번째 인자에는 몇 개의 대체를 진행할지를 지정하는 값을 전달합니다. 예를 들면 아래와 같습니다.

```Go
strings.Replace("Hello World", "World", "Go", 1)
```

위 코드는 "Hello World"라는 문자열에서 "World"를 "Go"로 대체하라는 의미입니다. 마지막 인자인 1은 한 번만 대체하라는 의미입니다. 만약 2로 설정하면 "Hello Go"가 되겠죠? 이와 비슷한 함수로는 `strings.ReplaceAll()` 함수가 있는데, 차이점은 대체하는 횟수를 지정할 필요가 없다는 것입니다.

더 복잡한 검색 및 대체 기능을 원한다면, 정규표현식을 사용할 수 있습니다. Go 언어에서는 `regexp` 패키지를 사용하여 정규표현식을 지원합니다. 아래 예제 코드는 "Hello World"라는 문자열에서 모음을 모두 대문자로 변경하는 예제입니다.

```Go
regex := regexp.MustCompile("[aeiou]")
output := regex.ReplaceAllString("Hello World", strings.ToUpper)
fmt.Println(output) // HEllO WOrld
```

## 깊이 파고들기

앞서 말한대로, Go 언어에서는 정규표현식을 지원합니다. 정규표현식은 다양한 문자열 패턴을 찾는데 사용되는 강력한 도구입니다. 이를 활용하면 원하는 패턴을 가진 문자열을 찾고 대체할 수 있습니다. 정규표현식은 다소 복잡하고 난해할 수 있지만, 검색 및 대체 기능을 최적화 하는데 매우 유용합니다.

더 자세한 내용을 알고 싶다면, Go 공식 문서에 있는 정규표현식 패키지 설명을 읽어보세요. 여기에는 다양한 예제와 유용한 팁이 포함되어 있어서 더 나은 검색 및 대체 기능을 만들 수 있을 것입니다.

## 관련 링크

https://golang.org/pkg/strings/#Replace // `strings.Replace()` 함수의 공식 문서

https://golang.org/pkg/strings/#ReplaceAll // `strings.ReplaceAll()` 함수의 공식 문서

https://golang.org/pkg/regexp/ // 정규표현식 패키지의 공식 문서