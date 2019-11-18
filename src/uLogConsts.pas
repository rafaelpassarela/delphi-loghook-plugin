unit uLogConsts;

interface

const
  C_DEFAULT_LOG_EXT = '.log';
  C_DEFAULT_BKP_EXT = '.bkp';
  C_DEFAULT_IMG_EXT = '.jpg';
  C_DEFAULT_LOG_DIR = 'Logs\';
  C_DEFAULT_IMG_DIR = 'Images\';
  C_DEFAULT_IGNORE_LIST = ';EResNotFound;EAbort;EDBClient;EExternalException;EIB_ISCError;EDatabaseError;EIdSocketError;EIdConnClosedGracefully;'; // ;Exception;
  C_DEFAULT_LOG_SIZE = 1024;
  C_DEFAULT_LIFETIME = 7;
  C_DEFAULT_IMAGE_QUALITY = 50;

  C_LOGSIZE_LIMIT = 10240;

  C_SECTION_CONFIG = 'LogConfig';
  C_SECTION_LOG = 'Log';

  C_IDENT_ACTIVE         = 'Active';
  C_IDENT_LOG_FILES_DIR  = 'LogOutDir';
  C_IDENT_IGNORELIST     = 'IgnoreLogTypes';
  C_IDENT_IGNOREMESSAGES = 'IgnoreLogMsges';
  C_IDENT_LOGSIZE        = 'LogSize';
  C_IDENT_COUNT          = 'LogCount';
  C_IDENT_CHECKINGERROR  = 'TratandoErro';
  C_IDENT_ERRORDETECTED  = 'ErrorDetected';
  C_IDENT_LOGLIFETIME    = 'LogLifeTime';
  C_IDENT_GET_IMAGE      = 'CanCreateJPEG';
  C_IDENT_IMAGE_QUALITY  = 'JPEGQuality';

//  ConstErroHostFB = 'Unable to complete network request to host';
//  ConstErroTCPClient = '<<>>Connection Closed Gracefully';

implementation

end.
