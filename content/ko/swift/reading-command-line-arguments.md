---
title:                "컴퓨터 프로그래밍: 명령줄 인수 읽기"
html_title:           "Swift: 컴퓨터 프로그래밍: 명령줄 인수 읽기"
simple_title:         "컴퓨터 프로그래밍: 명령줄 인수 읽기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# 이게 뭐고 왜 필요해?

커맨드 라인 인수 읽기는 프로그래밍에서 사용되는 기술로, 프로그램을 실행시킬 때 입력한 인수를 읽어와서 프로그램에서 사용할 수 있도록 해줍니다. 이를 통해 사용자가 프로그램을 사용할 때 입력한 값을 받아올 수 있게 됩니다.

# 방법:

```swift
import Foundation

// Step 1: main 함수에서 command line arguments 읽기
let arguments = CommandLine.arguments

// Step 2: arguments 출력하기
print("입력한 인수는 \(arguments) 입니다.")
```

# 깊이 있는 정보

(1) 커맨드 라인 인수 읽기는 과거에는 C 언어에서 많이 사용되었지만, 현재는 Swift와 같은 현대적인 언어들에서도 많이 사용되고 있습니다.

(2) 커맨드 라인 인수를 읽는 방법 외에도 환경 변수를 읽는 방법이 있습니다. 이는 arguments와 유사하게 사용할 수 있지만, arguments는 프로그램 실행 시에만 유효한 값이고, 환경 변수는 시스템 전체에서 유효한 값입니다.

(3) CommandLine 클래스를 사용하여 arguments를 읽어올 수 있지만, low-level의 C API를 사용하는 방법도 있습니다.

# 더 알아보기

- [CommandLine Class Documentation](https://developer.apple.com/documentation/foundation/commandline)
- [Swift.org](https://swift.org/)