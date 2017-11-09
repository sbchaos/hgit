module Data.Storage.ParserSpec where

import Test.Hspec
import Test.QuickCheck

import Text.ParserCombinators.Parsec
import Data.Storage.Parser
import Data.Storage.ObjectTypes

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
--main :: IO ()
--main = hspec spec
treeData = "tree 1282\NUL100644 .gitmodules\NUL\230\157\226\155\178\209\214CK\139)\174wZ\216\194\228\140S\145\&100644 CHANGELOG.mkd\NUL\236\NUL\167`aS\156\247taG\136'\STX\DC4I\150\150\248q100644 DEVELOPERS.mkd\NUL\249Z\175\128\NUL}\"_\NUL\211\DLE\153\135\238B\239,.\f\n100644 LICENSE\NUL\238\b\215\228O\NAK\DLE\142\245\&5\149P9\157\173U\149[V\202\&100644 README.md\NUL\209\142\233E\STXQ\234\ESC\154\STX\235\212\214\252\224\"\223\158\181\228\&40000 adobe-swatches-solarized\NUL\EM\129\199h\129\198\161N\DC4\208g\164BG\172\209\191k\188:40000 apple-colorpalette-solarized\NUL\130\\s+\221:b\174\181C\202\137\STXj&\162\238\SI\186&40000 emacs-colors-solarized\NUL{\171((\223]\226\&2b\168!\204H\254\f\207\139\210\169\174\&40000 files\NUL\245\254\140> \178Wr#\246\ETBh:R\234\195\FS\\\159\&040000 gedit\NUL[`\DC1\NAK\DLE\219\179\216V\f\245\138\&6\162\n\153\252\ETBVX40000 gimp-palette-solarized\NUL`\201\223=n\EM\148\183mr\192a\160&9\175=\146VU40000 img\NUL\151\156\244\&7R\228\214\152\199\181\180|\255fQB\162t\193\&340000 intellij-colors-solarized\NUL?\246\212\&10;f\204P\228[o\171\215#\STX\242\DLE\174\188\&40000 iterm2-colors-solarized\NUL\143\&8zS\SUB\208\143\DC4l\134\228\182\NUL{\137\128d\173M\DEL40000 mutt-colors-solarized\NUL\RS7Y.b\200Y\t\190L^^\183t\241wvn\132\"40000 netbeans-colors-solarized\NUL\143\&2\US\145p@\217\ETX\247\SOH\162\179:\238\226j\237.\229D40000 osx-terminal.app-colors-solarized\NUL\r@\132e\130\b\"\246\162\175\204\244>\150'7_\237\194x40000 putty-colors-solarized\NULc\223\166\196\r!O\142\SIv\211\159z\"\131\224S\148\n\EM40000 qtcreator\NULE9!\162g\211\235\133^@\199\222s\174\228`\136V?>40000 seestyle-colors-solarized\NUL]\214\131*2A\135\248\245!\190\249(\137\US\184|\248E\246\&40000 textmate-colors-solarized\NUL<\NAK\151>\209\a\231\179}\FSH\133\248)\132e\142\203\223j40000 textwrangler-bbedit-colors-solarized\NULM\177R\179jG\227\SUB\135.w\140\STX\SYN\USSx\136\228K40000 tmux\NUL\t\181\242\246\158\NAK\150\198\255f\251\CAN~\166\189\195\133\132QR40000 utils\NULc^\187\185\EM\252\187\175o\233X\153\133S\191?_\224\146\DLE40000 vim-colors-solarized\NUL\184z!\NUL\176\167\148$\205K*NN\240\&2t\177\&0\162\ACK40000 visualstudio-colors-solarized\NUL\141\234q\144\183\156\ENQ@J\166\161\240\214|\\fq\214o\225\&40000 xchat\NUL\nS\CAN&\233\DC3\164\177\CAN#\238\ESC\230\225\179g\248&\NULo40000 xfce4-terminal\NUL(p\189\243\148\166\182\179\189\DLE\194c\255\233\&9j\r=3f40000 xresources\NUL]\SUB!./\217\205\194\182x\227\190V\207wk/\SYN\207\226"
goTree = GoTree 1282 [TreeEntry {mode = "100644", path = ".gitmodules", hash = "\230\157\226\155\178\209\214CK\139)\174wZ\216\194\228\140S\145"},TreeEntry {mode = "100644", path = "CHANGELOG.mkd", hash = "\236\NUL\167`aS\156\247taG\136'\STX\DC4I\150\150\248q"},TreeEntry {mode = "100644", path = "DEVELOPERS.mkd", hash = "\249Z\175\128\NUL}\"_\NUL\211\DLE\153\135\238B\239,.\f\n"},TreeEntry {mode = "100644", path = "LICENSE", hash = "\238\b\215\228O\NAK\DLE\142\245\&5\149P9\157\173U\149[V\202"},TreeEntry {mode = "100644", path = "README.md", hash = "\209\142\233E\STXQ\234\ESC\154\STX\235\212\214\252\224\"\223\158\181\228"},TreeEntry {mode = "40000 ", path = "adobe-swatches-solarized", hash = "\EM\129\199h\129\198\161N\DC4\208g\164BG\172\209\191k\188:"},TreeEntry {mode = "40000 ", path = "apple-colorpalette-solarized", hash = "\130\\s+\221:b\174\181C\202\137\STXj&\162\238\SI\186&"},TreeEntry {mode = "40000 ", path = "emacs-colors-solarized", hash = "{\171((\223]\226\&2b\168!\204H\254\f\207\139\210\169\174"},TreeEntry {mode = "40000 ", path = "files", hash = "\245\254\140> \178Wr#\246\ETBh:R\234\195\FS\\\159\&0"},TreeEntry {mode = "40000 ", path = "gedit", hash = "[`\DC1\NAK\DLE\219\179\216V\f\245\138\&6\162\n\153\252\ETBVX"},TreeEntry {mode = "40000 ", path = "gimp-palette-solarized", hash = "`\201\223=n\EM\148\183mr\192a\160&9\175=\146VU"},TreeEntry {mode = "40000 ", path = "img", hash = "\151\156\244\&7R\228\214\152\199\181\180|\255fQB\162t\193\&3"},TreeEntry {mode = "40000 ", path = "intellij-colors-solarized", hash = "?\246\212\&10;f\204P\228[o\171\215#\STX\242\DLE\174\188"},TreeEntry {mode = "40000 ", path = "iterm2-colors-solarized", hash = "\143\&8zS\SUB\208\143\DC4l\134\228\182\NUL{\137\128d\173M\DEL"},TreeEntry {mode = "40000 ", path = "mutt-colors-solarized", hash = "\RS7Y.b\200Y\t\190L^^\183t\241wvn\132\""},TreeEntry {mode = "40000 ", path = "netbeans-colors-solarized", hash = "\143\&2\US\145p@\217\ETX\247\SOH\162\179:\238\226j\237.\229D"},TreeEntry {mode = "40000 ", path = "osx-terminal.app-colors-solarized", hash = "\r@\132e\130\b\"\246\162\175\204\244>\150'7_\237\194x"},TreeEntry {mode = "40000 ", path = "putty-colors-solarized", hash = "c\223\166\196\r!O\142\SIv\211\159z\"\131\224S\148\n\EM"},TreeEntry {mode = "40000 ", path = "qtcreator", hash = "E9!\162g\211\235\133^@\199\222s\174\228`\136V?>"},TreeEntry {mode = "40000 ", path = "seestyle-colors-solarized", hash = "]\214\131*2A\135\248\245!\190\249(\137\US\184|\248E\246"},TreeEntry {mode = "40000 ", path = "textmate-colors-solarized", hash = "<\NAK\151>\209\a\231\179}\FSH\133\248)\132e\142\203\223j"},TreeEntry {mode = "40000 ", path = "textwrangler-bbedit-colors-solarized", hash = "M\177R\179jG\227\SUB\135.w\140\STX\SYN\USSx\136\228K"},TreeEntry {mode = "40000 ", path = "tmux", hash = "\t\181\242\246\158\NAK\150\198\255f\251\CAN~\166\189\195\133\132QR"},TreeEntry {mode = "40000 ", path = "utils", hash = "c^\187\185\EM\252\187\175o\233X\153\133S\191?_\224\146\DLE"},TreeEntry {mode = "40000 ", path = "vim-colors-solarized", hash = "\184z!\NUL\176\167\148$\205K*NN\240\&2t\177\&0\162\ACK"},TreeEntry {mode = "40000 ", path = "visualstudio-colors-solarized", hash = "\141\234q\144\183\156\ENQ@J\166\161\240\214|\\fq\214o\225"},TreeEntry {mode = "40000 ", path = "xchat", hash = "\nS\CAN&\233\DC3\164\177\CAN#\238\ESC\230\225\179g\248&\NULo"},TreeEntry {mode = "40000 ", path = "xfce4-terminal", hash = "(p\189\243\148\166\182\179\189\DLE\194c\255\233\&9j\r=3f"},TreeEntry {mode = "40000 ", path = "xresources", hash = "]\SUB!./\217\205\194\182x\227\190V\207wk/\SYN\207\226"}]

spec :: Spec
spec =
  describe "Parser" $ do
    describe "git type" $ do
      it "parses the tree to proper object" $
        parse gitType "test" "tree" `shouldBe` Right GTree
      it "parses the blob to proper object" $
        parse gitType "test" "blob" `shouldBe` Right GBlob
    describe "header" $
      it "parser header to tuple value" $
        parse header "test" "tree 12\0" `shouldBe` Right (GTree, 12)
    describe "reads blob content" $
      it "parses content" $
        parse (blob 5) "test" "abcdefgh" `shouldBe` Right "abcde"
    describe "reads tree enty" $ do
      it "parses entry successfully" $
        parse entry "test" "100644 .gitmodules\NUL\230\157\226\155\178\209\214CK\139)\174wZ\216\194\228\140S\145\&" `shouldBe` Right TreeEntry { mode="100644", hash="\230\157\226\155\178\209\214CK\139)\174wZ\216\194\228\140S\145\&", path=".gitmodules"}
      it "parses tree successfully" $
        parseGitObject treeData `shouldBe` Right goTree
