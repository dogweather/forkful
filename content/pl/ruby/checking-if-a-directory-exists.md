---
title:    "Ruby: Sprawdzanie istnienia katalogu"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Dlaczego warto sprawdzać istnienie katalogu w programowaniu Ruby?

Sprawdzanie istnienia katalogu jest ważną częścią programowania w Ruby, ponieważ pozwala nam na tworzenie bezpieczniejszych i bardziej niezawodnych aplikacji. Dzięki temu możemy upewnić się, że nasz kod będzie działał poprawnie, nawet jeśli katalog, którego używamy, nie istnieje. Dodatkowo, jest to również ważne przy pracy z plikami lub tworzeniu nowych katalogów.

## Jak to zrobić?

Sprawdzenie, czy dany katalog istnieje, można wykonać za pomocą metody `exist?` oraz klasy `Dir`. Należy pamiętać, że metoda `exist?` przyjmuje jako argument ścieżkę do katalogu, który chcemy sprawdzić. Przykładowy kod wyglądałby następująco:

```ruby
# Sprawdzenie istnienia katalogu "MojeKatalogi"

if Dir.exist?("MojeKatalogi")
  puts "Ten katalog istnieje."
else
  puts "Ten katalog nie istnieje."
end
```

W powyższym przykładzie, jeśli katalog "MojeKatalogi" istnieje, to zostanie wyświetlona informacja o jego istnieniu. W przypadku, gdyby katalog ten nie istniał, zostanie wyświetlona informacja o jego braku.

## Głębszy wgląd

Warto również zauważyć, że metoda `exist?` jest dostępna tylko dla obiektów klasy `Dir`, dlatego przed jej użyciem należy upewnić się, że odpowiednia klasa jest załadowana. Dla większej przejrzystości kodu, można również użyć metody `directory?`, która pozwala na zweryfikowanie, czy dany obiekt jest katalogiem.

## Zobacz również

Jeśli chcesz dowiedzieć się więcej na temat sprawdzania istnienia katalogów w Ruby, polecam przeczytać poniższe artykuły:

- [Dokumentacja Ruby](https://ruby-doc.org/core-2.7.2/Dir.html#method-c-exist-3F)
- [Blog Ruby Inside](https://rubyinside.com/how-to-check-if-a-directory-exists-in-ruby-9.html)
- [Poradnik Udemy](https://blog.udemy.com/ruby-check-if-directory-exists/)