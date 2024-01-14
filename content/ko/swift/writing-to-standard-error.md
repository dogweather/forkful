---
title:    "Swift: 표준 오류에 쓰는 것"
keywords: ["Swift"]
---

{{< edit_this_page >}}

# 왜
오류를 기록하는 것이 중요한 이유는 디버깅을 도와준다는 것입니다.

# 하는 법
기본 개념
기본적으로 코드 작성 중에 오류가 발생하면 콘솔 로그에 오류 메시지가 출력되는데, 이를 stderr에 출력하도록 변경해주어야 합니다. 이는 디버깅을 더욱 효과적으로 도와줄 수 있습니다.

```Swift

import Foundation
import Darwin

// 글을 stderr에 출력하는 함수
func writeError(_ message: String) {
    fputs(message, stderr)
}

// 함수 실행
writeError("에러가 발생했습니다.")

```

출력 결과는 다음과 같을 것입니다.

`에러가 발생했습니다.`

# 딥 다이브
Glibc, CLLibc 및 다른 표준 라이브러리는 대부분 경고 및 오류에 대해서는 stderr에 출력하도록 설정되어 있습니다. 그러나 사용자 커스텀 라이브러리에서는 이를 직접 설정해주어야 합니다. 배열에 존재하지 않는 index에 접근하여 코드를 실행하게 되면서 오류가 발생한다고 가정해봅시다. 이 경우에도 오류 메시지가 stdout에 출력되지 않고 stderr에 출력되도록 설정해야 합니다.

# 더 알아보기
[Swift 공식 문서 - 파일에 쓰기](https://docs.swift.org/swift-book/LanguageGuide/Functions.html#ID520)

# 참고
[Swift-Korea - 디버깅에 대한 이해](https://swift-korea.github.io/2016/09/22/debugging-in-swift.html)