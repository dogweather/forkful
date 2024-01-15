---
title:                "스트링 대문자화"
html_title:           "Ruby: 스트링 대문자화"
simple_title:         "스트링 대문자화"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 왜?
우리는 때때로 입력된 문자열을 대문자로 바꿀 필요가 있습니다. 누군가의 이름을 대문자로 표기해야 할 때, 또는 특정 키워드를 찾기 위해 대문자로 변환해야 할 때 등 다양한 경우가 있을 수 있습니다. 이러한 요구를 충족하기 위해 루비는 간단한 방법을 제공합니다.

## 방법
```Ruby
string = "hello world"
puts string.upcase
```

위 예제에서는 문자열 `"hello world"`를 대문자로 변환한 후 출력하는 코드를 보여줍니다. `upcase` 메소드를 사용하여 문자열을 대문자로 바꿀 수 있습니다. 또한 `capitalize` 메소드를 사용하면 첫 번째 문자만 대문자로 바꿀 수 있습니다.

```Ruby
string = "hello world"
puts string.capitalize
```

출력 결과는 다음과 같습니다:

```
Hello world
```

위 예제에서는 문자열의 첫 번째 문자인 "h" 만 대문자로 변환되어 출력되었습니다.

## 깊이있는 탐구
루비에서 `upcase`와 `capitalize` 메소드는 `String` 클래스의 일부분입니다. 따라서 `String` 클래스의 소스 코드를 살펴보면 이해가 더욱 쉽습니다.

`upcase` 메소드는 `String` 클래스 내부에 다음과 같이 구현되어 있습니다:

```Ruby
def upcase
  self.tr(lowercase, uppercase)
end
```

여기서 `tr`은 주어진 문자열을 다른 문자열로 변환하는 메소드입니다. 즉, `upcase`는 문자열을 전부 대문자로 변환하는 것과 같은 기능을 합니다.

`capitalize` 메소드는 다음과 같이 구현되어 있습니다:

```Ruby
def capitalize
  self[0, 1].upcase + self[1..-1].downcase
end
```

여기서 `[0, 1]`은 문자열의 첫 번째 문자를 나타냅니다. 따라서 첫 번째 문자는 `upcase` 메소드를 사용하여 대문자로 변환됩니다. 그리고 `downcase` 메소드를 사용하여 두 번째 문자부터 문자열의 끝까지는 소문자로 변환됩니다.

이처럼 `upcase`와 `capitalize` 메소드는 간단한 방식으로 문자열을 대문자로 변환할 수 있게 해줍니다.

## 더 알아보기
여러분은 이제 문자열을 대문자로 변환하는 간단한 방법을 배웠습니다. 하지만 `String` 클래스에는 더 다양한 메소드가 있으니 궁금하다면 루비 공식 문서를 참고해보세요.

## 더 알아보기
- [루비 공식 문서](https://ruby-doc.org/)
- [위키백과 루비 페이지](https://ko.wikipedia.org/wiki/루비_(프로그래밍_언어))