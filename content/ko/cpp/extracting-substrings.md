---
title:    "C++: 서브스트링 추출"
keywords: ["C++"]
---

{{< edit_this_page >}}

### 왜

C++ 프로그래밍을 하는 개발자라면 문자열을 처리하는 일은 피할 수 없습니다. 때로는 문자열에서 원하는 부분만 추출해야 할 때가 있습니다. 이번 포스트에서는 C++로 문자열의 일부를 추출하는 방법에 대해 알아보겠습니다.

### 방법

우선, 문자열에서 원하는 부분을 추출하기 위해서는 `substr()` 함수를 사용해야 합니다. 이 함수는 문자열에서 지정된 위치부터 지정된 길이만큼의 부분을 추출해 줍니다. 다음은 `substr()` 함수를 사용한 예제 코드와 출력 결과입니다.

```C++
// 문자열 생성
string str = "Hello World";

// 문자열의 일부를 추출해 새로운 문자열 생성
string sub_str = str.substr(3, 5);

// 새로운 문자열 출력
cout << sub_str << endl;
```

출력 결과:

```bash
lo Wo
```

위와 같은 방법으로 `substr()` 함수를 이용하여 문자열의 일부를 추출할 수 있습니다. 또한 `substr()` 함수의 시작 위치를 생략하면 첫 번째 문자부터 추출하고, 길이를 생략하면 시작 위치부터 문자열 끝까지 추출합니다.

### 깊이 파고들기

`substr()` 함수를 사용할 때 주의해야 할 점이 있습니다. 만약 시작 위치가 문자열의 길이보다 큰 경우, 오류가 발생하지 않고 빈 문자열을 반환합니다. 이 점을 유의하여 사용해야 합니다. 또한 `substr()` 함수를 이용하여 원본 문자열에서 추출한 부분을 변경하더라도 원본 문자열의 내용은 변하지 않습니다.

### 참고

- [C++ `substr()` 함수 참고 문서](https://www.cplusplus.com/reference/string/string/substr/)
- [C++ 문자열 처리 관련 포스트](https://blog.naver.com/PostView.nhn?blogId=username&logNo=123456789) 
- [C++ 문자열 다루기 강좌](https://www.youtube.com/watch?v=123456789)