---
title:    "C: 텍스트 파일 작성하기"
keywords: ["C"]
---

{{< edit_this_page >}}

컴퓨터 프로그래밍은 현대 사회에서 필수적인 기술이 되었습니다. 그중에서도 C 언어는 많은 개발자들에게 사랑받고 쓰여지고 있습니다. 오늘은 C 언어를 사용해 텍스트 파일을 작성하는 방법을 살펴보겠습니다.

# Why
텍스트 파일을 작성하는 가장 큰 이유는 데이터를 저장하고 공유하기 위해서입니다. 프로그램을 작성하거나 작업 내용을 보다 효율적으로 관리하기 위해서는 텍스트 파일이 필수입니다. 따라서, 텍스트 파일 작성에 대한 기초적인 지식은 개발자로서 필수적입니다.

# How To
텍스트 파일을 작성하는 가장 간단한 방법은 `fopen()` 함수를 사용하는 것입니다. 이 함수는 파일을 열 수 있도록 파일 포인터를 반환합니다. 그 후, `fprintf()` 함수를 사용하여 파일에 텍스트를 기록할 수 있습니다. 아래는 간단한 예제 코드입니다.

```C
FILE *fp; // 파일 포인터 변수 선언

fp = fopen("sample.txt", "w"); // 파일을 쓰기 모드로 열기

fprintf(fp, "Hello, world!"); // 파일에 텍스트 기록

fclose(fp); // 파일 닫기
```

위 코드를 실행하면 `sample.txt`라는 파일에 "Hello, world!"라는 텍스트가 기록됩니다.

# Deep Dive
텍스트 파일을 작성할 때 중요한 점은 파일을 어떤 모드로 열고 데이터를 어떻게 기록할 것인지를 결정하는 것입니다. 예제 코드에서 사용한 `w` 모드는 쓰기 모드로, 이미 파일이 존재하는 경우 이전 내용을 덮어쓰게 됩니다. 만약 파일이 없으면 새로운 파일이 생성됩니다. 이외에도 `r` 모드(읽기 모드), `a` 모드(추가 모드) 등 다양한 모드가 있으니 필요에 따라 적절하게 사용하시면 됩니다.

See Also
- [C 언어 기본적인 파일 입출력](https://modoocode.com/231)
- [한글을 포함한 텍스트 파일 쓰기](http://blog.naver.com/PostView.nhn?blogId=ndb796&logNo=220769121770&categoryNo=0&parentCategoryNo=0&viewDate=&currentPage=1&postListTopCurrentPage=1&from=postView)
- [C 코딩 스타일 가이드](http://wiki.hash.kr/index.php/%EC%BD%94%EB%94%A9_%EC%BD%94%EB%94%A9_%EC%8A%A4%ED%83%80%EC%9D%BC_Guide)