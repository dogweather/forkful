---
title:                "새 프로젝트 시작하기"
date:                  2024-01-20T18:04:39.977729-07:00
model:                 gpt-4-1106-preview
simple_title:         "새 프로젝트 시작하기"

category:             "Ruby"
tag:                  "Getting Started"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
새 프로젝트를 시작한다는 건 빈 화면에서 코딩을 시작해 완전히 새로운 것을 만드는 것입니다. 프로그래머들은 아이디어를 현실화하고 문제를 풀기 위해 이렇게 합니다.

## How to: (어떻게 하나요?)
```Ruby
# Ruby 현재 버전을 사용하여 간단한 프로젝트 구조를 만들기
# 1. 새 디렉터리 생성
Dir.mkdir("my_new_project")

# 2. Gemfile을 만들고 Bundler로 관리 시작
File.open("my_new_project/Gemfile", "w") do |file|
  file.puts "source 'https://rubygems.org'"
  file.puts "gem 'rspec'"
end

# 3. Bundler 사용하여 설치
Dir.chdir("my_new_project") do
  system("bundle install")
end

# 출력 결과
# => Fetching gem metadata from https://rubygems.org/...
# => Resolving dependencies...
# => Using rspec 3.10.0
# => Bundle complete! 1 Gemfile dependency, 28 gems now installed.
# => Use `bundle info [gemname]` to see where a bundled gem is installed.
```

## Deep Dive (심층 탐구)
새 프로젝트를 시작할 때 아래 단계를 거칩니다.

- **프로젝트 구조**: 파일과 폴더의 구조는 나중에 확장성과 유지 보수성에 큰 영향을 미칩니다.
- **버전 관리**: Git 같은 버전 관리 시스템은 변경 사항의 추적과 협업을 용이하게 합니다.
- **의존성 관리**: Bundler, RVM, rbenv 등은 프로젝트의 Ruby 버전과 gem 의존성을 관리합니다.

역사적으로 Ruby는 특히 웹 개발에 강점을 가졌습니다. Rails와 같은 프레임워크는 생산성과 깨끗한 코드 작성을 극대화합니다. 하지만 다른 도구와 프레임워크도 많으니 상황에 맞게 선택해야 합니다.

## See Also (참고 자료)
- Ruby 공식 사이트: [https://www.ruby-lang.org/ko/](https://www.ruby-lang.org/ko/)
- Bundler 공식 문서: [https://bundler.io/docs.html](https://bundler.io/docs.html)
- RVM 공식 사이트: [https://rvm.io/](https://rvm.io/)
- rbenv GitHub 저장소: [https://github.com/rbenv/rbenv](https://github.com/rbenv/rbenv)
- Ruby on Rails 가이드: [https://guides.rubyonrails.org/](https://guides.rubyonrails.org/)
