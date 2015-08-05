Pod::Spec.new do |s|
  s.name         = "ejdb"
  s.version      = "1.2.10"
  s.summary      = "Embedded JSON Database engine (CMake is REQUIRED to install)"
  s.homepage     = "https://github.com/Softmotions/ejdb"
  s.license      = { :type => "LGPL", :file => "LICENSE" }
  s.authors      = "Softmotions"
  s.social_media_url = 'https://twitter.com/ejdblab'
  s.description = <<-DESC
        EJDB is the embeddable (in-process) database engine designed for
        querying collections of JSON documents and persisting data. EJDB is
        implemented as a C library based on Tokyo Cabinet key-value storage
        engine. EJDB design is inspired by Mongodb and follows the same
        philosophy , especially for the query language
  DESC

  s.source       = { :git => "https://github.com/Softmotions/ejdb.git", :tag => s.version }
  s.source_files = "includes/*.h", "src/{bson,ejdb,tcbdb,tcfdb,tchdb,tctdb,tcutil}/*.{h,c}", "src/*.h", "src/tcutil/nix/platform.h", "build/src/generated/*.h"
  s.exclude_files = "src/**/tests/*.{h,c}", "src/**/tools/*.{h,c}"
  #s.public_header_files = "build/src/generated/basedefs.h", "src/bson/bson.h", "src/ejdb/{ejdb,ejdb_private}.h", "src/tcbdb/tcbdb.h", "src/tcfdb/tcfdb.h", "src/tchdb/tchdb.h", "src/tdtdb/tdtdb.h", "src/tcutil/tcutil.h"
  s.public_header_files = "includes/*.h"

  s.ios.deployment_target = "8.0"
  s.osx.deployment_target = "10.10"

  s.compiler_flags = '-I$(PODS_ROOT)/ejdb/src/tcutil'

  #s.private_header_files = "src/**/{ejdbutl.h,encoding.h,md5.h,nxjson.h,platform.h,utf8proc.h}", "build/src/generated/myconf.h"
  s.libraries = 'z'


  #s.header_mappings_dir = 'src'
  s.preserve_paths = "src/tcutil/inc", "src/tcutil/nix"

  s.prepare_command = <<-CMD
      rm -fr build
      mkdir build
      cd build
      cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=out ..
      cd ..

      mkdir includes
      cp build/src/generated/basedefs.h src/bson/bson.h src/ejdb/ejdb.h src/ejdb/ejdb_private.h src/tcbdb/tcbdb.h src/tcfdb/tcfdb.h src/tchdb/tchdb.h src/tctdb/tctdb.h src/tcutil/tcutil.h  includes
  CMD

end
