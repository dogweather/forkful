---
title:    "Gleam: 패턴과 일치하는 문자 삭제"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

Gleam로 문자열 매칭 패턴 삭제하는 방법

## 왜

문자열 매칭 패턴을 삭제하는 방법은 프로그래밍에서 빈번하게 사용되는 기술입니다. 예를 들어, 사용자가 입력한 불필요한 문자를 제거하거나, 특정 패턴과 일치하지 않는 문자열을 제거하는 등의 작업을 할 때 이용할 수 있습니다.

## 어떻게

아래는 Gleam로 문자열 매칭 패턴을 삭제하는 방법을 코딩 예제와 함께 보여주는 코드 블록입니다. 

```Gleam
String.replace("가나다라마바사", ["라", "마"], "")
// 출력 결과: 가나다바사
```

위 예제는 "가나다라마자"라는 문자열에서 "라"와 "마"라는 패턴을 찾아 삭제하는 예제입니다. String.replace 함수를 사용하여 검색할 문자열, 삭제할 패턴 및 삭제할 대체 문자를 입력하면 됩니다.

## 자세히 살펴보기

문자열 매칭 패턴을 삭제하는 방법은 Gleam의 기본 함수 중 하나인 String.replace를 이용합니다. 이 함수는 입력받은 문자열에서 해당하는 패턴을 검색하여 삭제할 수 있도록 도와줍니다. 또한 여러 개의 패턴을 동시에 삭제할 수 있도록 배열로 입력하면 됩니다.

추가적으로, String.replace 함수의 옵션을 활용하여 대소문자 구분 여부나 정규식 사용 여부를 설정할 수 있습니다. 이를 통해 더 정교한 문자열 처리가 가능합니다.

## 한 마무리로

이제 당신도 Gleam으로 문자열 매칭 패턴을 간편하게 삭제할 수 있게 되었습니다. 다양한 옵션과 기능들을 활용하여 복잡한 문자열 처리도 손쉽게 해결할 수 있습니다. 

## 관련 자료

관련된 자료들을 참고하시면 더욱 강력한 문자열 처리를 할 수 있습니다.

- Gleam 공식 문서: [String.replace 함수](https://gleam.run/documentation/stdlib/string.html#replace)
- Gleam 샘플 코드: [파일 이름에서 숫자 제거](https://gleam.run/play/#pub:gleam_stdlib/filename?load=false)
- Replacing text in strings with Gleam [블로그 글](https://daltonfairbanks.com/swap-text-with-gleam/)