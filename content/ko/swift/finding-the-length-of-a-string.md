---
title:    "Swift: 문자열 길이 찾기"
keywords: ["Swift"]
---

{{< edit_this_page >}}

# 왜

문자열의 길이를 찾는 것에 대해 관심을 가지는 이유는 다양합니다. 첫째, 문자열의 길이는 문자열을 다룰 때 매우 유용한 정보입니다. 둘째, Swift에서 문자열의 길이를 찾는 방법은 매우 쉽고 간단합니다.

# 어떻게

Swift에서 문자열의 길이를 찾는 방법은 두 가지가 있습니다. 첫 번째 방법은 `count` 메소드를 사용하는 것입니다. 예를 들어, 다음과 같이 문자열의 길이를 찾을 수 있습니다.

```
Swift let str = "안녕하세요"
print(str.count)
// Output: 5
```

또 다른 방법은 `string` 속성을 사용하는 것입니다. 예를 들어,

```
Swift let str = "Hello there!"
print(str.length)
// Output: 12
```

# 딥 다이브

문자열의 길이를 찾을 때 알아둘 몇 가지 중요한 점이 있습니다. 첫째, 문자열에는 `count`와 `length` 모두 포함된다는 것입니다. 둘째, 한글이나 다른 언어의 복합 문자들의 경우, `count` 메소드와 `length` 속성이 다를 수 있습니다. 따라서 어떤 문자열을 다룰 때는 사전에 어떤 메소드나 속성을 사용할 지 정하는 것이 중요합니다.

# 참고

- [Swift 문자열의 길이 구하기](https://blog.eunjin.kr/post/swift-string-length)
- [Swift 문자열 다루기](https://hyerios.tistory.com/entry/Swift-%EB%AC%B8%EC%9E%90%EC%97%B4-%EB%8B%A4%EB%A3%A8%EA%B8%B0)
- [Swift 문자열 다루기 팁](https://www.swiftdevcenter.com/swift-string-%EB%8B%A4%EB%A3%A8%EA%B8%B0-%ED%89%B4%EC%8A%A4%EC%81%98%EB%A9%B4-%EC%BB%AC%EB%A0%89%EC%85%98-%EC%B0%A8%EC%9D%B4%EA%B0%80-%EB%90%98%EC%97%88%EB%84%A4-%EC%97%AC%EB%A6%84-%EB%82%B4%EA%B0%80-%EC%B0%BE%EC%9C%BC%EB%A9%B4/)

# 참고

- [Swift 문자열의 길이 구하기](https://blog.eunjin.kr/post/swift-string-length)
- [Swift 문자열 다루기](https://hyerios.tistory.com/entry/Swift-%EB%AC%B8%EC%9E%90%EC%97%B4-%EB%8B%A4%EB%A3%A8%EA%B8%B0)
- [Swift 문자열 다루기 팁](https://www.swiftdevcenter.com/swift-string-%EB%8B%A4%EB%A3%A8%EA%B8%B0-%ED%89%B4%EC%8A%A4%EC%81%98%EB%A9%B4-%EC%BB%AC%EB%A0%89%EC%85%98-%EC%B0%A8%EC%9D%B4%EA%B0%80-%EB%90%98%EC%97%88%EB%84%A4-%EC%97%AC%EB%A6%84-%EB%82%B4%EA%B0%80-%EC%B0%BE%EC%9C%BC%EB%A9%B4/)