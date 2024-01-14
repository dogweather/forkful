---
title:                "Swift: 명령줄 인수 읽기"
simple_title:         "명령줄 인수 읽기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# 왜
이 포스트에서는 소프트웨어 개발에서 중요한 역할을 하는 명령 줄 인수를 읽는 방법을 배우게 됩니다. 명령 줄 인수는 여러분이 작성하는 프로그램에 대한 사용자 입력을 제공하는 방법 중 하나입니다. 이 포스트를 읽는 것으로 더 나은 개발자가 되는 첫 걸음을 내딛을 수 있습니다.

# 방법
명령 줄 인수를 읽기 위해서는 `CommandLine` 클래스를 사용합니다. 이 클래스에는 `arguments` 속성이 있으며, 이를 통해 모든 인수를 배열로 받아올 수 있습니다. 아래는 예시 코드입니다.

```Swift
if CommandLine.arguments.count > 0 {
    for argument in CommandLine.arguments {
        print(argument)
    }
}
```

위 코드를 실행하면, 사용자가 입력한 모든 인수가 출력됩니다. 예를 들어, `./myProgram Swift is awesome`를 입력하면, `./myProgram`, `Swift`, `is`, `awesome`가 순서대로 출력됩니다.

# 깊이 파고들기
명령 줄 인수는 여러분이 다양한 방법으로 활용할 수 있습니다. 예를 들어, 사용자가 입력한 인수에 따라 다른 동작을 하도록 프로그램을 제어할 수 있습니다. 또한, 인수를 사용하여 특정 파일 경로나 설정값을 지정할 수도 있습니다. 이 외에도 다양한 가능성이 존재합니다. 따라서 명령 줄 인수를 잘 활용하는 것은 좋은 프로그램을 개발하는 데 꼭 필요한 기술입니다.

# 또 보기
- [Swift 공식 문서](https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html)
- [레이 캐스트: 명령 줄 인수](https://www.raywenderlich.com/7678174-command-line-programs-on-macos-tutorial)
- [The Swift Dev: 명령 줄 인수](https://theswiftdev.com/all-about-command-line-arguments-in-swift/)