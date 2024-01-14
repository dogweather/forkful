---
title:    "Haskell: 텍스트 파일 쓰기"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

# 왜?

텍스트 파일을 쓰는 이유는 여러 가지가 있습니다. 일반적으로는 프로그래머들이 작성한 코드나 데이터를 저장하고 공유하기 위해서 사용됩니다. 또한, 텍스트 파일은 버전 관리 시스템과 같은 다른 도구들과 호환될 수 있으므로 코드 작성과 관리에 유용합니다.

# 방법

텍스트 파일을 기록하는 방법은 매우 간단합니다. 먼저, `writeFile` 함수를 사용하여 새로운 파일을 생성할 수 있습니다. 다음은 이 파일에 쓸 내용을 작성하는 것입니다. 마지막으로, `do` 블록을 사용하여 이를 실행하고 파일을 닫아줍니다.

```Haskell
main = do
    writeFile "hello.txt" "안녕하세요!"
```

위의 코드는 "hello.txt"라는 새로운 파일을 생성하고 그 안에 "안녕하세요!"라는 내용을 작성합니다. 이렇게 작성된 파일은 현재 디렉토리에 저장됩니다. 만약 이미 존재하는 파일에 내용을 추가하고 싶다면 `appendFile` 함수를 사용할 수 있습니다.

# 깊은 곳으로

텍스트 파일을 작성하는 데 더 많은 옵션과 기능이 있습니다. 예를 들어, 인코딩을 지정하거나 줄바꿈 문자를 설정할 수 있습니다. 또한, `readFile` 함수를 사용하여 이미 존재하는 파일의 내용을 읽어올 수도 있습니다. 텍스트 파일을 작성하고 읽는 것은 Haskell 프로그래밍에서 가장 보편적인 기능 중 하나이므로 잘 이해하고 숙달해두는 것이 좋습니다.

# 더 알아보기

- [Haskell 공식 문서](https://www.haskell.org/documentation/)
- [Haskell 프로그래밍 언어 소개](https://codingalwayssmiling.blogspot.com/2016/06/hello-haskell.html)
- [Haskell: 입문부터 기본까지](https://lifeprogrammer.tistory.com/17)
- [Haskell 입문자를 위한 가이드](http://wiki.hash.kr/index.php?title=Haskell_%EC%9E%85%EB%AC%B8%EC%9E%90%EB%A5%BC_%EC%9C%84%ED%95%9C_%EA%B0%80%EC%9D%B4%EB%93%9C)