## Участие в курса

За да участвате в курса, е препоръчително да предприемете следните действия:

0. Да имате GitHub акаунт
1. Включване в Discord сървъра за основния курс чрез [линка от мудъл](https://learn.fmi.uni-sofia.bg/mod/url/view.php?id=TODO)
2. Следване на инструкциите описани в Discord канала `#насоки-и-правила`
    1. Като подточка, трябва да изберете поне `Практикум` от `Channels & Roles`. Целта на това е да мога да ви пращам известия и новини относно курса.
3. Трябва да ми изпратите на лично съобщение или в Discord или по друг канал следните данни, по предпочитание форматирани по следния начин:
    ```
    <фн>,<две имена на кирилица>,<github потребителско име>
    ```
    Нужни са ми за да мога да формирам таблица с точки за всеки човек, както и да направя индивидуалните хранилища, които ще се използват за предаване и обратна връзка по домашните.

    Пример за правилно изпратени данни:
    ```
    62555,Павел Атанасов,reo101
    ```

Действията 0. и 3. са задължителни за участие в курса, но 1. и 2. са силно препоръчителни.

## Discord

Практикумът споделя Discord сървър с главния курс по ФП.

Целите на сървъра са следните:
1. Комуникация от моя страна към вас - обявления, пояснения
2. Директна асинхронна чат комуникация с мен - "как да го направя това?" и подобни
3. Комункация помежду ви - има канали в които могат да се случват дискусии свързани с курса (а и извън него)

В Discord можете да ме намерите под името `Павел Атанасов` (`reo101`).
Не се притеснявайте да ми пишете на лично съобщение при всякакви изникнали въпроси

## Контакти

* Mail - pavel.atanasov2001+fmi-fp-lab@gmail.com
* Discord - `@reo101`

## Взимане на курса

Курсът се взима с домашни и проект.

Домашните не са задължителни за завършването на курса, а проектът е.

Отбелязвам че имането на проект **не гарантира** взимането на предмета.
Пример за кога би се случило това е ако не напишете нито едно домашно и изберете да правите проект, който дава минималния брой точки.

Домашните обикновено са между 3 и 7 на брой, като практиката ми показва че ми стига времето за 3-4.

Домашните ще се предават в лично github хранилище за всеки човек чрез pull request.

Проектът, който правите, може да е споделен между практикума и главната дисциплина, стига той да е за поне `n` точки, където `n` се уточнява с мен и Трифон когато стане време за проекти.

Проектът се предава по същия начин както домашните - pull request във вашето хранилище в github, по който ви пиша обратна връзка.

## Технически детайли

### Версия на компилатора

В курса ще работим с версия `9.4.7` на Haskell-ският компилатор (`ghc`). Съответно трябва когато инсталирате компилатора да внимавате да е `9.4.7`

Инсрукции за как да си инсталирате най-важните Haskell-ски инструменти има в следващата секция.

### Инсталиране на инструменти за работа с Haskell

Препоръчваният начин е да използвате [`ghcup`][ghcup] - инструмент за менежиране на инструменти свързани с Haskell.

На [началната страница на `ghcup`][ghcup] има едноредови инструкции за инсталирането на `ghcup`, заедно с gif, демонстриращ как да го използвате, за да инсталирате инструменти.

Алтернативно можете да разгледате [по-подробните инструкции](https://www.haskell.org/ghcup/install/) за инсталиране на `ghcup`.

След като го инсталирате, разгледайте [инсрукциите за използване на `ghcup`](https://www.haskell.org/ghcup/guide/), но накратко

```
> ghcup install ghc 9.4.7
> ghcup install cabal-install
> ghcup install haskell-language-server
```

Алтернативно, ако сте под Linux, можете да използвате `ghcup tui` за да получите интерактивен текстов интърфейс (TUI) за работа с `ghcup`.

След това, моля проверете дали работи всичко като изпълните командата `ghci` в терминал. Би трябвало да видите нещо такова:
```
GHCi, version 9.4.7: https://www.haskell.org/ghc/  :? for help
```
Обърнете внимание на версията - трябва да е 9.4.7.

При спънки не се безпокойте да се свържете с мен.

[ghcup]: https://www.haskell.org/ghcup/

### Редактор и интеграция с Haskell

Препоръчаният метод за работа с Haskell е [VSCode](https://code.visualstudio.com/), заедно с [HLS](https://marketplace.visualstudio.com/items?itemName=haskell.haskell) разширението.

То включва много полезни функционалности - често срещани неща като "форматирай", "отиди на дефиниция", "какъв е типът на това", "оцени израз" и т.н.

Важно е да се отбележи, че докато работим с файл извън "проект" (което ще правим повечето време), е **нужно** да имаме инсталирано `ghc` и да е в `PATH`, за да работи HLS

Нямам против да ползвате <моя-любим-редактор> - аз ще ползвам `vim` докато ви показвам неща, като най-вероятно можете да си нагласите и HLS да работи с <моя-любим-редактор>. В такъв случай, можете лесно да се сдобиетe със HLS чрез `ghcup`.

### Форматиране

В курса ще форматираме кода си чрез [`ormolu`](https://github.com/tweag/ormolu).

Най-лесният начин да постигнете това е използвайки разширението към редактора си.

За VSCode - след като имате Haskell разширението от миналата стъпка, би трябвало да е достатъчно да [пуснете автоматичното форматиране при записване на файл(Format on Save)](https://stackoverflow.com/a/54665086) от VSCode настройките.
След това, при всяко записване на файла с който работите, би следвало кодът ви да се форматира.

## Haskell ресурси

* Донякъде плагиатствам от [този курс](https://github.com/bobatkey/CS316-2022)
* [Книгата](http://www.cs.nott.ac.uk/~pszgmh/pih.html) на която е базиран горният курс (и също я смятам за добър ресурс)
* [Силно препоръчително четиво][parse-dont-validate] свързано с
  * как да ни се налага да мислим по-малко докато програмираме
  * как да избягваме големи класове грешки с помощта на компилатора
  * ключова начин на мислене в Haskell
  * защо да програмираме на Haskell
* [Hoogle](https://hoogle.haskell.org/) - търсене за хаскел функции (идентификатори) (и по типове!)
* [Hackage](http://hackage.haskell.org/) - търсене за хаскел пакети
* [Real World Haskell](http://book.realworldhaskell.org/) - практично насочена

  Малко остаряла.

* Специализирани ресурси:

  * защо да правим `a -> Maybe b`, вместо `a -> Bool`:
    * [Parse, don't validate][parse-dont-validate]
    * https://runtimeverification.com/blog/code-smell-boolean-blindness/
    * https://existentialtype.wordpress.com/2011/03/15/boolean-blindness/
  * Програмиране с типове - [Thinking with Types](https://thinkingwithtypes.com/)
  * Паралелно и конкуретно програмиране - [Parallel and Concurrent Programming in Haskell](https://simonmar.github.io/pages/pcph.html)
    * една от въведителните части е доста добра за добиване на по-добро разбиране над оценителния модел на Haskell
  * Разглеждане на фундаментални типови класове - [Typeclassopedia](https://wiki.haskell.org/Typeclassopedia)
  * Разглеждане на различни интересни библиотеки/разшиерния на езика - [24 days of \*](https://ocharles.org.uk/)
  * Има **много** научни статии, които са доста лесно четими дори за начинаещи - ще споделям периодично някои.

Съветвам ви директно да ме питате за повече ресурси ако ви интересува конкретна тема.

[parse-dont-validate]: https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/