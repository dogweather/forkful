---
title:                "Swift: 스트링 길이 찾기"
simple_title:         "스트링 길이 찾기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# 왜
문자열의 길이를 찾는 것에 대해 관심을 가질 이유는 여러 가지가 있습니다. 예를 들어, 사용자가 입력한 문자열의 길이를 확인하고 입력 글자 수에 따라서 다른 작업을 수행할 수 있습니다. 또는 문자열의 길이를 기준으로 문장 출력 형식을 다르게 할 수도 있습니다.

# 어떻게
````Swift
let string = "안녕하세요"
let length = string.count
print(length)
````
코드 블록 안에서는 다음과 같은 과정이 진행됩니다. 첫째로, 문자열을 변수에 할당합니다. 그리고 count 메서드를 사용해 해당 문자열의 길이를 찾은 다음 그 값을 변수에 할당합니다. 마지막으로는 해당 값을 출력해주는 것입니다.

출력 결과는 5가 나오게 됩니다. 왜냐하면 한글은 여러 바이트를 차지하기 때문에 단순히 문자 개수를 세는 것이 아니라, 바이트 단위로 길이를 계산해야 하기 때문입니다.

# 더 깊이 들어가기
문자열의 길이를 계산하는 방법에 대해 더 자세히 알아보겠습니다. 문자열의 길이를 찾는 데에는 여러 가지 방법이 있지만, 가장 간단하고 빠른 방법은 count 메서드를 사용하는 것입니다. 그러나 문자열의 길이를 찾을 때는 알파벳이나 숫자만으로 이루어진 단순한 문자열이라면 count 메서드를 사용해도 전체 길이를 잘못 계산할 수 있습니다.

예를 들어, "Hello, world!" 라는 문자열의 길이를 찾을 때는 count 메서드를 사용해도 13이라는 결과가 나오게 됩니다. 하지만 실제로는 띄어쓰기를 포함한 총 12개의 문자를 가진 문자열이므로, count 메서드로는 정확한 길이를 찾을 수 없습니다.

따라서 문자열의 길이를 계산할 때는 문자열 내에 존재하는 모든 문자를 고려해야 합니다. 이를 위해 정규표현식과 같은 기술을 사용해 원하는 문자열의 패턴을 찾아내고, 그 패턴에 맞는 길이를 계산하는 방법을 사용할 수 있습니다.

# 봐봐
[Swift Strings - Apple Developer Documentation](https://developer.apple.com/documentation/swift/string)

[String Length Using Count and Length Property in Swift](https://www.tutorialspoint.com/swift-program-to-calculate-length-of-a-string)

[How to Calculate the Length of a String in Swift](https://www.hackingwithswift.com/example-code/strings/how-to-calculate-the-length-of-a-string-in-swift)